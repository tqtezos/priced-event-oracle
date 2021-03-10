
$ tezos-client --wait none originate contract VestingTez \
  transferring 0 from $BOB_ADDRESS running \
  "$(cat contracts/vesting_tez.tz | tr -d '\n')" \
  --init 'Pair (Pair "tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr" "tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm") (Pair 0 (Pair "2020-09-10T18:57:56Z" (Pair 30 1)))' \
  --burn-cap 0.753





```bash
❯❯❯ alpha-client --wait none originate contract ExplicitAdminOracle \
  transferring 0 from $BOB_ADDRESS running \
  "$(cat explicit_admin_oracle.tz | tr -d '\n')" \
  --init "(Pair \"$BOB_ADDRESS\" 1)" \
  --burn-cap 0.15675 --force
Waiting for the node to be bootstrapped before injection...
Current head: BL6hes6E5etq (timestamp: 2021-01-27T20:54:48-00:00, validation: 2021-01-27T20:55:16-00:00)
Node is bootstrapped, ready for injecting operations.
Estimated gas: 2904.152 units (will add 100 for safety)
Estimated storage: 627 bytes added (will add 20 for safety)
Operation successfully injected in the node.
Operation hash is 'onoXiqUymyA8CimRN5MKVj7TWgQS7Mkb4yden8Dq9o3oTag4g1a'
NOT waiting for the operation to be included.
Use command
  tezos-client wait for onoXiqUymyA8CimRN5MKVj7TWgQS7Mkb4yden8Dq9o3oTag4g1a to be included --confirmations 30 --branch BMQDPZZBXpryijdvRqicny8f1Zn3hSTrxbVGbGZ27PPbAXY5YQB
and/or an external block explorer to make sure that it has been included.
This sequence of operations was run:
  Manager signed operations:
    From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
    Fee to the baker: ꜩ0.000914
    Expected counter: 526611
    Gas limit: 3005
    Storage limit: 647 bytes
    Balance updates:
      tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ............. -ꜩ0.000914
      fees(tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU,194) ... +ꜩ0.000914
    Origination:
      From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
      Credit: ꜩ0
      Script:
        { parameter
            (or (pair %ask (contract (pair string nat)) string)
                (or (or (mutez %setPrice) (address %setOwner)) (mutez %withdraw))) ;
          storage (pair (address %owner) (mutez %price)) ;
          code { DUP ;
                 CAR ;
                 DIP { CDR } ;
                 IF_LEFT
                   { DROP ;
                     DUP ;
                     CDR ;
                     AMOUNT ;
                     COMPARE ;
                     EQ ;
                     IF { NIL operation } { PUSH string "wrong price" ; FAILWITH } }
                   { SWAP ;
                     DUP ;
                     CAR ;
                     SENDER ;
                     COMPARE ;
                     EQ ;
                     IF { SWAP ;
                          IF_LEFT
                            { IF_LEFT { SWAP ; CAR } { SWAP ; CDR ; SWAP } ;
                              PAIR ;
                              NIL operation }
                            { SENDER ;
                              CONTRACT unit ;
                              IF_NONE
                                { PUSH string "not receiver" ; FAILWITH }
                                { SWAP ; UNIT ; TRANSFER_TOKENS ; NIL operation ; SWAP ; CONS } } }
                        { PUSH string "not admin" ; FAILWITH } } ;
                 PAIR } }
        Initial storage: (Pair "tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm" 1)
        No delegate for this contract
        This origination was successfully applied
        Originated contracts:
          KT1CPftoA5VyL6GDzeNtYsekpJa3ztmf2Bdg
        Storage size: 370 bytes
        Paid storage size diff: 370 bytes
        Consumed gas: 2904.152
        Balance updates:
          tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ... -ꜩ0.0925
          tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ... -ꜩ0.06425

New contract KT1CPftoA5VyL6GDzeNtYsekpJa3ztmf2Bdg originated.
Contract memorized as ExplicitAdminOracle.
```

```bash
❯❯❯ alpha-client --wait none originate contract LambdaAdminOracle \
  transferring 0 from $BOB_ADDRESS running \
  "$(cat lambda_admin_oracle.tz | tr -d '\n')" \
  --init "(Pair \"$BOB_ADDRESS\" 1)" \
  --burn-cap 0.13325

Waiting for the node to be bootstrapped before injection...
Current head: BLYrtActb7nm (timestamp: 2021-01-27T20:48:48-00:00, validation: 2021-01-27T20:49:17-00:00)
Node is bootstrapped, ready for injecting operations.
Estimated gas: 2516.822 units (will add 100 for safety)
Estimated storage: 533 bytes added (will add 20 for safety)
Operation successfully injected in the node.
Operation hash is 'op12EyE34PD7qiNc4gvVE8mt2eaxv1BMHzPfdrUsZSjSCQ2QAZe'
NOT waiting for the operation to be included.
Use command
  tezos-client wait for op12EyE34PD7qiNc4gvVE8mt2eaxv1BMHzPfdrUsZSjSCQ2QAZe to be included --confirmations 30 --branch BMN4QAGkHGzSEGbjsZvkt4dCj4ZRnFDnMBsyj7Uz1trCDqjAh9h
and/or an external block explorer to make sure that it has been included.
This sequence of operations was run:
  Manager signed operations:
    From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
    Fee to the baker: ꜩ0.000781
    Expected counter: 526610
    Gas limit: 2617
    Storage limit: 553 bytes
    Balance updates:
      tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ............. -ꜩ0.000781
      fees(tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU,194) ... +ꜩ0.000781
    Origination:
      From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
      Credit: ꜩ0
      Script:
        { parameter
            (or (pair %ask (contract (pair string nat)) string)
                (lambda %admin unit (pair (list operation) (pair (address %owner) (mutez %price))))) ;
          storage (pair (address %owner) (mutez %price)) ;
          code { DUP ;
                 CAR ;
                 DIP { CDR } ;
                 IF_LEFT
                   { DROP ;
                     DUP ;
                     CDR ;
                     AMOUNT ;
                     COMPARE ;
                     EQ ;
                     IF { NIL operation ; PAIR } { PUSH string "wrong price" ; FAILWITH } }
                   { SWAP ;
                     DUP ;
                     CAR ;
                     SENDER ;
                     COMPARE ;
                     EQ ;
                     IF { DROP ; UNIT ; EXEC } { PUSH string "not admin" ; FAILWITH } } } }
        Initial storage: (Pair "tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm" 1)
        No delegate for this contract
        This origination was successfully applied
        Originated contracts:
          KT1Jxgwa7oZLpwUG2mnCw3DoGKhqKUsZvaB3
        Storage size: 276 bytes
        Paid storage size diff: 276 bytes
        Consumed gas: 2516.822
        Balance updates:
          tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ... -ꜩ0.069
          tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ... -ꜩ0.06425

New contract KT1Jxgwa7oZLpwUG2mnCw3DoGKhqKUsZvaB3 originated.
Contract memorized as LambdaAdminOracle.
```


```
❯❯❯ alpha-client --wait none originate contract Receiver \
  transferring 0 from $BOB_ADDRESS running \
  "parameter (pair string nat); storage unit; code{FAILWITH}" \
  --burn-cap 0.07325
Waiting for the node to be bootstrapped before injection...
Current head: BMKFe4XpBr7W (timestamp: 2021-01-27T21:05:18-00:00, validation: 2021-01-27T21:05:49-00:00)
Node is bootstrapped, ready for injecting operations.
Estimated gas: 1573.704 units (will add 100 for safety)
Estimated storage: 293 bytes added (will add 20 for safety)
Operation successfully injected in the node.
Operation hash is 'oo4rUKYsKN14EgBmUMfzzF1XHe8jaH6RnTUiG6Nhedec4gszCJF'
NOT waiting for the operation to be included.
Use command
  tezos-client wait for oo4rUKYsKN14EgBmUMfzzF1XHe8jaH6RnTUiG6Nhedec4gszCJF to be included --confirmations 30 --branch BLkJXwVJCFNVFoL8NY8xaEp5DGiZfd7BKe32V28duKrCqoYdEPX
and/or an external block explorer to make sure that it has been included.
This sequence of operations was run:
  Manager signed operations:
    From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
    Fee to the baker: ꜩ0.000433
    Expected counter: 526612
    Gas limit: 1674
    Storage limit: 313 bytes
    Balance updates:
      tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ............. -ꜩ0.000433
      fees(tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU,194) ... +ꜩ0.000433
    Origination:
      From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
      Credit: ꜩ0
      Script:
        { parameter (pair string nat) ; storage unit ; code { FAILWITH } }
        Initial storage: Unit
        No delegate for this contract
        This origination was successfully applied
        Originated contracts:
          KT1HHgfN2v47ogDikDLRKigaXaDAW2rSdyk9
        Storage size: 36 bytes
        Paid storage size diff: 36 bytes
        Consumed gas: 1573.704
        Balance updates:
          tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ... -ꜩ0.009
          tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ... -ꜩ0.06425

New contract KT1HHgfN2v47ogDikDLRKigaXaDAW2rSdyk9 originated.
Contract memorized as Receiver.
```


Call explicit oracle

```bash
❮❮❮ alpha-client --wait none transfer 0.000001 from $BOB_ADDRESS to KT1CPftoA5VyL6GDzeNtYsekpJa3ztmf2Bdg \
  --entrypoint ask --arg 'Pair "KT1HHgfN2v47ogDikDLRKigaXaDAW2rSdyk9" "my_query"'

Waiting for the node to be bootstrapped before injection...
Current head: BMPFui6Eo6EY (timestamp: 2021-01-27T21:07:48-00:00, validation: 2021-01-27T21:08:07-00:00)
Node is bootstrapped, ready for injecting operations.
Estimated gas: 4470.531 units (will add 100 for safety)
Estimated storage: no bytes added
Operation successfully injected in the node.
Operation hash is 'oo1C5cpyT4ES8hjvUriRen3vJa7ZT5cN1KVmMygp8b58vC2yf5z'
NOT waiting for the operation to be included.
Use command
  tezos-client wait for oo1C5cpyT4ES8hjvUriRen3vJa7ZT5cN1KVmMygp8b58vC2yf5z to be included --confirmations 30 --branch BMPFui6Eo6EYFrnj5s1iBzZMCycjYX8rPLaGBpwwotwDWTgP5qm
and/or an external block explorer to make sure that it has been included.
This sequence of operations was run:
  Manager signed operations:
    From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
    Fee to the baker: ꜩ0.000773
    Expected counter: 526613
    Gas limit: 4571
    Storage limit: 0 bytes
    Balance updates:
      tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ............. -ꜩ0.000773
      fees(tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU,194) ... +ꜩ0.000773
    Transaction:
      Amount: ꜩ0.000001
      From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
      To: KT1CPftoA5VyL6GDzeNtYsekpJa3ztmf2Bdg
      Entrypoint: ask
      Parameter: (Pair "KT1HHgfN2v47ogDikDLRKigaXaDAW2rSdyk9" "my_query")
      This transaction was successfully applied
      Updated storage:
        (Pair 0x0000aad02222472cdf9892a3011c01caf6407f027081 1)
      Storage size: 370 bytes
      Consumed gas: 4470.531
      Balance updates:
        tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ... -ꜩ0.000001
        KT1CPftoA5VyL6GDzeNtYsekpJa3ztmf2Bdg ... +ꜩ0.000001
```

Call lambda oracle

```bash
❯❯❯ alpha-client --wait none transfer 0.000001 from $BOB_ADDRESS to KT1Jxgwa7oZLpwUG2mnCw3DoGKhqKUsZvaB3 \
  --entrypoint ask --arg 'Pair "KT1HHgfN2v47ogDikDLRKigaXaDAW2rSdyk9" "my_query"'

Waiting for the node to be bootstrapped before injection...
Current head: BM9erbaF7BvS (timestamp: 2021-01-27T21:08:48-00:00, validation: 2021-01-27T21:08:52-00:00)
Node is bootstrapped, ready for injecting operations.
Estimated gas: 4083.389 units (will add 100 for safety)
Estimated storage: no bytes added
Operation successfully injected in the node.
Operation hash is 'opEqLi92PYEfooqyAiqroez1qw17hwn1j9HyyVfmxTH1uovtkfw'
NOT waiting for the operation to be included.
Use command
  tezos-client wait for opEqLi92PYEfooqyAiqroez1qw17hwn1j9HyyVfmxTH1uovtkfw to be included --confirmations 30 --branch BM9erbaF7BvSge1yoQDL8GqDo4yrHEcs1dcrSmTid7HDHRqVPdP
and/or an external block explorer to make sure that it has been included.
This sequence of operations was run:
  Manager signed operations:
    From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
    Fee to the baker: ꜩ0.000734
    Expected counter: 526614
    Gas limit: 4184
    Storage limit: 0 bytes
    Balance updates:
      tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ............. -ꜩ0.000734
      fees(tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU,194) ... +ꜩ0.000734
    Transaction:
      Amount: ꜩ0.000001
      From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
      To: KT1Jxgwa7oZLpwUG2mnCw3DoGKhqKUsZvaB3
      Entrypoint: ask
      Parameter: (Pair "KT1HHgfN2v47ogDikDLRKigaXaDAW2rSdyk9" "my_query")
      This transaction was successfully applied
      Updated storage:
        (Pair 0x0000aad02222472cdf9892a3011c01caf6407f027081 1)
      Storage size: 276 bytes
      Consumed gas: 4083.389
      Balance updates:
        tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ... -ꜩ0.000001
        KT1Jxgwa7oZLpwUG2mnCw3DoGKhqKUsZvaB3 ... +ꜩ0.000001
```

