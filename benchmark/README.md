How to:

helper benchmarks:
```bash
elm-optimize-level-2 --optimize-speed --output=helpers/main.js src/HelpersMain.elm
cd helpers && serve
```

full runs benchmarks:
```bash
elm-optimize-level-2 --optimize-speed --output=main.js src/Main.elm
serve
```
to get comparable times, make sure to eliminate e.g. open apps and other tabs
and run in Chromium/Google Chrome.
