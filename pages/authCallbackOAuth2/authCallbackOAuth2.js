// @ts-check
const ERR = require('async-stacktrace');
const assert = require('assert');
const Sentry = require('@prairielearn/sentry');
const express = require('express');
const router = express.Router();

const logger = require('../../lib/logger');
const config = require('../../lib/config');
const csrf = require('../../lib/csrf');
const sqldb = require('@prairielearn/postgres');

const { OAuth2Client } = require('google-auth-library');

router.get('/', function (req, res, next) {
  if (!config.hasOauth) return next(new Error('Google login is not enabled'));
  const code = req.query.code;
  if (code == null) {
    return next(new Error('No "code" query parameter for authCallbackOAuth2'));
  } else if (typeof code !== 'string') {
    return next(new Error(`Invalid "code" query parameter for authCallbackOAuth2: ${code}`));
  }
  // FIXME: should check req.query.state to avoid CSRF
  let oauth2Client, identity;
  try {
    oauth2Client = new OAuth2Client(
      config.googleClientId,
      config.googleClientSecret,
      config.googleRedirectUrl
    );
  } catch (err) {
    ERR(err, next);
    return;
  }
  logger.verbose('Got Google auth with code: ' + code);
  oauth2Client.getToken(code, function (err, tokens) {
    if (err?.response) {
      // This is probably a detailed error from the Google API client. We'll
      // pick off the useful bits and attach them to the Sentry scope so that
      // they'll be included with the error event.
      Sentry.configureScope((scope) => {
        scope.setContext('OAuth', {
          code: err.code,
          data: err.response.data,
        });
      });
    }
    if (ERR(err, next)) return;
    try {
      logger.verbose('Got Google auth tokens: ' + JSON.stringify(tokens));
      oauth2Client.credentials = tokens;
      // tokens.id_token is a JWT (JSON Web Token)
      // http://openid.net/specs/draft-jones-json-web-token-07.html
      // A JWT has the form HEADER.PAYLOAD.SIGNATURE
      // We get the PAYLOAD, un-base64, parse to JSON:
      const parts = tokens.id_token.split('.');
      identity = JSON.parse(Buffer.from(parts[1], 'base64').toString('utf-8'));
      logger.verbose('Got Google auth identity: ' + JSON.stringify(identity));
      assert(identity.email);
    } catch (err) {
      ERR(err, next);
      return;
    }
    const params = [
      identity.email, // uid
      identity.name || identity.email, // name (use email if name is not present)
      identity.sub, // uin
      'Google', // provider
    ];
    sqldb.call('users_select_or_insert', params, (err, result) => {
      if (ERR(err, next)) return;
      const tokenData = {
        user_id: result.rows[0].user_id,
        authn_provider_name: 'Google',
      };
      const pl_authn = csrf.generateToken(tokenData, config.secretKey);
      res.cookie('pl_authn', pl_authn, {
        maxAge: config.authnCookieMaxAgeMilliseconds,
        httpOnly: true,
        secure: true,
      });
      let redirUrl = res.locals.homeUrl;
      if ('preAuthUrl' in req.cookies) {
        redirUrl = req.cookies.preAuthUrl;
        res.clearCookie('preAuthUrl');
      }
      res.redirect(redirUrl);
    });
  });
});

module.exports = router;
