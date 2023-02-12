ALl the bash scripts in this directory should be called by one of the following command:

- calm sh fedora xxx
- calm sh darwin xxx
- calm sh msys xxx

All the scripts are executed in the `CALM_DIR` directory,
with env `CALM_DIR` and `APP_DIR` provided,
if you would like to have the relative path of `pwd`, please `cd "${APP_DIR}"` in your scripts.
