# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to [PVP Versioning](https://pvp.haskell.org/).

## [Unreleased]

## [0.4.6.0] - 2020-10-06

## Changed

- expose verifyJWT and use it in two places [@domenkozar]
- support GHC 8.10 [@domenkozar]
- move ToJWT/FromJWT to servant-auth [@erewok]
- #165 fix AnySite with Cookie 3.5.0 [@odr]

## [0.4.5.1] - 2020-02-06

## Changed

- #158 servant 0.17 support [@phadej]

## [0.4.5.0] - 2019-12-28

## Changed
- #144 servant 0.16 support and drop GHC 7.10 support [@domenkozar]
- #148 removed unused constaint in HasServer instance for Auth 
- #154 GHC 8.8 support [@phadej]

### Added
- #141 Support Stream combinator [@domenkozar]
- #143 Allow servant-0.16 [@phadej]

## [0.4.4.0] - 2019-03-02

### Added
- #141 Support Stream combinator [@domenkozar]
- #143 Allow servant-0.16 [@phadej]

## [0.4.3.0] - 2019-01-17

## Changed
- #117 Avoid running auth checks unnecessarily [@sopvop]
- #110 Get rid of crypto-api dependency [@domenkozar]
- #130 clearSession: improve cross-browser compatibility [@domenkozar]
- #136 weed out bytestring-conversion [@stephenirl]

## [0.4.2.0] - 2018-11-05

### Added
- `Headers hs a` instance for AddSetCookieApi [@domenkozar]
- GHC 8.6.x support [@domenkozar]

## [0.4.1.0] - 2018-10-05

### Added
- #125 Allow setting domain name for a cookie [@domenkozar]

## Changed
- bump http-api-data to 0.3.10 that includes Cookie orphan instances previously located in servant-auth-server [@phadej]
- #114 Export `HasSecurity` typeclass [@rockbmb]

## [0.4.0.1] - 2018-09-23

### Security
- #123 Session cookie did not apply SameSite attribute [@domenkozar]

### Added
- #112 HasLink instance for Auth combinator [@adetokunbo]
- #111 Documentation for using hoistServer [@mschristiansen]
- #107 Add utility functions for reading and writing a key to a file [@mschristiansen]

## [0.4.0.0] - 2018-06-17

### Added
- Support GHC 8.4 by @phadej and @domenkozar
- Support for servant-0.14 by @phadej
- #96 Support for jose-0.7 by @xaviershay
- #92 add `clearSession` for logout by @plredmond and @3noch
- #95 makeJWT: allow setting Alg via defaultJWTSettings by @domenkozar
- #89 Validate JWT against a JWKSet instead of JWK by @sopvop

### Changed
- #92 Rename CSRF to XSRF by @plredmond and @3noch
- #92 extract 'XsrfCookieSettings' from 'CookieSettings' and make XSRF checking optional
  by @plredmond and @3noch
- #69 export SameSite by @domenkozar
- #102 Reuse Servant.Api.IsSecure instead of duplicating ADT by @domenkozar

### Deprecated
- #92 Renamed 'makeCsrfCookie' to 'makeXsrfCookie' and marked the former as deprecated
  by @plredmond and @3noc
- #92 Made several changes to the structure of 'CookieSettings' which will require
  attention by users who have modified the XSRF settings by @plredmond and @3noch

### Security
- #94 Force cookie expiration on serverside by @karshan

## [0.3.2.0] - 2018-02-21

### Added
- #76 Export wwwAuthenticatedErr and elaborate its annotation by @defanor
- Support for servant-0.14 by @phadej

### Changed
- Disable the readme executable for ghcjs builds by @hamishmack
- #84 Make AddSetCookieApi type family open by @qnikst
- #79 Make CSRF checks optional for GET requests by @harendra-kumar

## [0.3.1.0] - 2017-11-08

### Added
- Support for servant-0.12 by @phadej

## [0.3.0.0] - 2017-11-07

### Changed
- #47 'cookiePath' and 'xsrfCookiePath' added to 'CookieSettings' by @mchaver

## [0.2.8.0] - 2017-05-26

### Added
- #45 Support for servant-0.11 by @phadej

## [0.2.7.0] - 2017-02-11

### Changed
- #27 #41 'acceptLogin' and 'makeCsrfCookie' functions by @bts
