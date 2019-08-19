#!/usr/bin/env bash
erl -pa _build/default/lib/*/ebin \
    -boot start_sasl \
    -config erlybot \
    -eval "application:ensure_all_started(erlybot)"
