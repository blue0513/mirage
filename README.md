# WIP: Mirage

Set marks on the symbol & region to delete quickly, as if it was _mirage_.

## What's This

## Setup

`git clone` and edit your init.el as below.

```elisp
(add-to-list 'load-path "/pscp/to/mirage")
(require 'mirage)
```

## Usages

|function name|usage|
|---|:---:|
|mirage-on|put mark on region or thing-at-point|
|mirage-off-all|delete all the marked region or symbol|
|mirage-jump-next|jump to the next mark|
|mirage-jump-prev|jump to the previous mark|

## Customize
