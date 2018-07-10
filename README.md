# network-status

Query network status and receive updates when stuff changes. Uses
Netlink interface provided by Linux and (patched for now)
[rtnetlink-hs](https://github.com/formaltech/rtnetlink-hs/) library for
accessing it.

Build with `stack build` and test with `stack exec network-status`.
