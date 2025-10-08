# Deploy Vesting contract
## Prepare assets
### Open repl
Open project in repl:
```
$ cabal repl
```

### Serialize Validator
Serialize `Vesting` validator into `./assets/vesting.plutus` file:
```
Prelude VestingUtils> saveVesting
```
You should see output like:
```
Serialized script to: ./assets/vesting.plutus
```
if everything pass well.

### Serialize Vesting Datum
Serialize `Vesting` datum into `./assets/vesting-datum.json` file:
```
Prelude VestingUtils> saveVestingDatum "addr_test..qph9qtutwc9" "2025-02-16T12:03:17Z"
```
You should see output like:
```
Wrote data to: ./assets/vesting-datum.json
{
    "constructor": 0,
    "fields": [
        {
            "bytes": "4952d7..699e3d401b94"
        },
        {
            "int": 1739707397000
        }
    ]
}
```
if everything pass well.

### Serialize Vesting Redeemer
Serialize `Vesting` redeemer into `./assets/unit.json` file:
```
Prelude VestingUtils> saveVestingRedeemer
```
You should see output like:
```
Wrote data to: ./assets/unit.json
{
    "constructor": 0,
    "fields": []
}
```
if everything pass well.


### Deploy contract
You can use prepared script to send some ADA to contract address:
```
$ ./scripts/vest.sh --name alice -t 4c3766ac2..d598e9fede7f85#0

```
You should see output like:
```
Estimated transaction fee: 172541 Lovelace
Transaction successfully submitted. Transaction hash is:
{"txhash":"06c7a8dc67cc...4e8e93583b77"}
transaction id: 06c7a8dc67cc...4e8e93583b77
Cardanoscan: https://preview.cardanoscan.io/transaction/06c7a8dc67cc...4e8e93583b77

```
if everything pass well.

Now you can verify, that your transaction has been validated and added into block.
```
$ ./../../scripts/query-address.sh -f ./assets/vesting.addr

```
You should see output like:
```
Query address: addr_test1wp53ycwlfuuylz6ttxcwury3p9ejfsu5tpq3m8d57l7ad8cqpk94v
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
06c7a8dc67cc...4e8e93583b77     0        10000000 lovelace + TxOutDatumInline BabbageEraOnwardsConway (HashableScriptData "\216y\159X\FS\241\199sO\187z\255\237\ENQ<\243\201\215\144\132\&9$N\169\244C\184\DC33\STX\237>b\ESC\NUL\NUL\SOH\149\SO\164\&3\136\255" (ScriptDataConstructor 0 [ScriptDataBytes "\241\199sO\187z\255\237\ENQ<\243\201\215\144\132\&9$N\169\244C\184\DC33\STX\237>b",ScriptDataNumber 1739707397000]))
..
```

### Claim
You can use prepared script to claim ADA from contract:
```
 ./scripts/claim.sh -c fec34457f23df1c..2a81ef3f732f419#0 -n alice -t 06c7a8dc67cc...4e8e93583b77#0

```
You should see output like:
```
Transaction will be valid from slot: 85504025
Estimated transaction fee: 347619 Lovelace
Transaction successfully submitted. Transaction hash is:
{"txhash":"be17c0127116387..b6d0357dadbb6eacf45"}
transaction id: be17c0127116387..b6d0357dadbb6eacf45
Cardanoscan: https://preview.cardanoscan.io/transaction/be17c0127116387..b6d0357dadbb6eacf45

```