# test repository
https://github.com/thomashoneyman/purescript-halogen-portal/issues/11

## Changing dependencies
1. Edit Main.purs
2. Edit packages.dhall
3. Run the following command

```shell
rm -rf .spago output && spago upgrade-set && spago bundle-app
```

## bug occurs when
|                | portal latest | portal 5887589 |
| -------------- | ------------- | -------------- |
| store latest   | no            | no             |
| store new fork | no            | no             |
| store old fork | no            | yes            |