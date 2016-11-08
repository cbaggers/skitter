# skitter

skitter is a repl friendly event system for games.

### Breaking API change: 2016-09-30

All listeners now have 3 manadatory args before the slot key args. `(event timestamp tpref)`

`tpref` (third party reference) is either nil or whatever argument the user passed to `#'cepl:step-host`

This assumes that the host passed it on to skitter. Currently I expect Im the only one using this so I can say it does :p

I have updated `cepl.skitter` to use this field
