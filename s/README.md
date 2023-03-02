ALl the scripts in this directory are only meant to be used by CALM Developers. Although the scripts in `usr` directory are prepared for the end-users, but the end-users shall not aware of their existence.

All the scripts should NOT be executed directly, instead, use `calm` command, like this:

```bash
calm s usr macos init-msg.scpt
```

All the scripts are executed in the `CALM_HOME` directory,
with env `CALM_HOME` and `CALM_APP_DIR` provided,

if you want to have the relative path of `pwd`, please `cd "${CALM_APP_DIR}"` in your scripts.
