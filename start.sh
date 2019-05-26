#!/usr/bin/env bash
erl -pa ebin/ deps/*/ebin/ \
    -config erlybot \
    -eval "application:ensure_all_started(erlybot)"
