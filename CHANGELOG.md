# Revision history for bejnarkli

## 0.2.1.1 -- 2021-04-20

* Offer "overlay" and "overlays" flake outputs

## 0.2.1.0 -- 2021-01-14

* Add --listenaddress option

## 0.2.0.1 -- 2021-01-05

* Fix a circular dependency in systemd unit ordering.

## 0.2.0.0 -- 2020-12-09

* Change the blob directory configured in the NixOS module
  from /var/lib/bejnarkli/ to /var/lib/bejnarkli/blobs/,
  and make it available as /var/lib/bejnarkli-blobs/ for
  unprivileged users.

## 0.1.1.1 -- 2020-12-07

* nixpkgs 20.09 & aarch64-linux support, and a README


## 0.1.1.0 -- 2020-12-04

* Initial release
