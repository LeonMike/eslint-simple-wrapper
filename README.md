# eslint-simple-wrapper

## How to use

A new minor-mode named "eslint-simple-wrapper-mode" will be available. By activating it, on every save of the .js code, it will run the configured eslint executable located at ```eslint-simple-wrapper-node-modules-dir/node_modules/.bin```.

the ```eslint-simple-wrapper-node-modules-dir``` is a variable required to locate your ````node_modules```. Set it using ```(setq eslint-simple-wrapper-node-modules-dir YOUR_NODE_MODULES)```).

The errors will be reported in a new buffer ```*eslint-simple-wrapper-errors-list*```.

The highlight and tooltips are compatible with ```javascript-mode```, but js2-mode and js3-mode are not supported, yet. The error list will be functional though.
