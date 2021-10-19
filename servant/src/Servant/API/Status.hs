{-# LANGUAGE DataKinds #-}
-- Flexible instances is necessary on GHC 8.4 and earlier
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Servant.API.Status where

import GHC.TypeLits (KnownNat, natVal)
import Network.HTTP.Types.Status

-- | Retrieve a known or unknown Status from a KnownNat
statusFromNat :: forall a proxy. KnownNat a => proxy a -> Status
statusFromNat = toEnum . fromInteger . natVal

-- | Witness that a type-level natural number corresponds to a HTTP status code
class KnownNat n => KnownStatus n where
  statusVal :: proxy n -> Status

instance KnownStatus 100 where
  statusVal _ = status100

instance KnownStatus 101 where
  statusVal _ = status101

instance KnownStatus 200 where
  statusVal _ = status200

instance KnownStatus 201 where
  statusVal _ = status201

instance KnownStatus 202 where
  statusVal _ = status202

instance KnownStatus 203 where
  statusVal _ = status203

instance KnownStatus 204 where
  statusVal _ = status204

instance KnownStatus 205 where
  statusVal _ = status205

instance KnownStatus 206 where
  statusVal _ = status206

instance KnownStatus 300 where
  statusVal _ = status300

instance KnownStatus 301 where
  statusVal _ = status301

instance KnownStatus 302 where
  statusVal _ = status302

instance KnownStatus 303 where
  statusVal _ = status303

instance KnownStatus 304 where
  statusVal _ = status304

instance KnownStatus 305 where
  statusVal _ = status305

instance KnownStatus 307 where
  statusVal _ = status307

instance KnownStatus 308 where
  statusVal _ = status308

instance KnownStatus 400 where
  statusVal _ = status400

instance KnownStatus 401 where
  statusVal _ = status401

instance KnownStatus 402 where
  statusVal _ = status402

instance KnownStatus 403 where
  statusVal _ = status403

instance KnownStatus 404 where
  statusVal _ = status404

instance KnownStatus 405 where
  statusVal _ = status405

instance KnownStatus 406 where
  statusVal _ = status406

instance KnownStatus 407 where
  statusVal _ = status407

instance KnownStatus 408 where
  statusVal _ = status408

instance KnownStatus 409 where
  statusVal _ = status409

instance KnownStatus 410 where
  statusVal _ = status410

instance KnownStatus 411 where
  statusVal _ = status411

instance KnownStatus 412 where
  statusVal _ = status412

instance KnownStatus 413 where
  statusVal _ = status413

instance KnownStatus 414 where
  statusVal _ = status414

instance KnownStatus 415 where
  statusVal _ = status415

instance KnownStatus 416 where
  statusVal _ = status416

instance KnownStatus 417 where
  statusVal _ = status417

instance KnownStatus 418 where
  statusVal _ = status418

instance KnownStatus 422 where
  statusVal _ = status422

instance KnownStatus 426 where
  statusVal _ = status426

instance KnownStatus 428 where
  statusVal _ = status428

instance KnownStatus 429 where
  statusVal _ = status429

instance KnownStatus 431 where
  statusVal _ = status431

instance KnownStatus 500 where
  statusVal _ = status500

instance KnownStatus 501 where
  statusVal _ = status501

instance KnownStatus 502 where
  statusVal _ = status502

instance KnownStatus 503 where
  statusVal _ = status503

instance KnownStatus 504 where
  statusVal _ = status504

instance KnownStatus 505 where
  statusVal _ = status505

instance KnownStatus 511 where
  statusVal _ = status511
