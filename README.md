
# Priced Event Oracle

This repo includes the _Priced Event Oracle_, an implementation of a
two-way priced oracle that uses off-chain events to return data to the caller:

```haskell
parameter (or (pair %ask (contract (pair string
                                         nat))
                         string)
              (lambda %admin (pair address
                                   mutez)
                             (pair (list operation)
                                   (pair address
                                         mutez))));
storage (pair (address %owner)
              (mutez %price));
```

### Contract source

The Michelson source for this contract and any variants may be found in
[/contracts](/contracts).


## Specification

The contract stores its administrator `(address %owner)` and the current
`(mutez price)`.

It has two entrypoints:
- `ask`
  * Accepts a `pair` of:
    + The "return" or callback `contract` reference: `(contract (pair string nat))`
    + The query as a `string`
  * Rejects any calls that do not transfer the exact `price` in Tez to the contract
- `admin`
  * Accepts a `lambda` whose:
    + Input is the current `owner` `address` and `price` in `mutez`
    + Output is the new `owner`, new `price`, and any `operation`'s to emit
  * Rejects any `SENDER` other than the `owner`

## Use

1. A user calls the `ask` entrypoint with his `query` `string` and callback
  `contract` reference
2. An off-chain indexer detects the `ask` contract call and calculates a response
3. The oracle's host calls the `contract` reference from the `ask` with the
  `pair` of:
  - `(string %query_response)`
  - `(nat %counter)`
    + Used to distinguish responses to a repeated query
    + Equal to the number of _previously completed_ responses to this `query`
      and `contract`, i.e. it's `0` for the first response to a pair of `query`,
      `contract (pair string nat)`, `1` for the second response, etc.

## Verification

This contract has a formal proof in Coq using Mi-Cho-Coq.

A merge request containing it may be found [here](https://gitlab.com/tqgroup/mi-cho-coq/-/merge_requests/1).


## Demo on Delphinet


Contract on Delphi [better-call.dev](https://better-call.dev/delphinet/KT18rqmG2eV33cZYQR9JMDAtzf8yEp59Nnqm/operations)

Originate:

```bash
❯❯❯ alpha-client --wait none originate contract LambdaAdminOracle3 \
  transferring 0 from $BOB_ADDRESS running \
  "$(cat lambda_admin_oracle_2.tz | tr -d '\n')" \
  --init "(Pair \"$BOB_ADDRESS\" 1)" \
  --burn-cap 0.127
Waiting for the node to be bootstrapped before injection...
Current head: BMAk1ar2GRSF (timestamp: 2021-02-03T22:32:57-00:00, validation: 2021-02-03T22:33:15-00:00)
Node is bootstrapped, ready for injecting operations.
Estimated gas: 2483.822 units (will add 100 for safety)
Estimated storage: 508 bytes added (will add 20 for safety)
Operation successfully injected in the node.
Operation hash is 'opRb971eCbVJb9ZSU22Go4GKLtPDWUBJSXiWGSbkaZnvZLLk6a3'
NOT waiting for the operation to be included.
Use command
  tezos-client wait for opRb971eCbVJb9ZSU22Go4GKLtPDWUBJSXiWGSbkaZnvZLLk6a3 to be included --confirmations 30 --branch BMAk1ar2GRSFZQig3bcycqn865URmnY1jBjnYmZJ6Wg6x2puDgH
and/or an external block explorer to make sure that it has been included.
This sequence of operations was run:
  Manager signed operations:
    From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
    Fee to the baker: ꜩ0.000753
    Expected counter: 526617
    Gas limit: 2584
    Storage limit: 528 bytes
    Balance updates:
      tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ............. -ꜩ0.000753
      fees(tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU,204) ... +ꜩ0.000753
    Origination:
      From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
      Credit: ꜩ0
      Script:
        { parameter
            (or (pair %ask (contract (pair string nat)) string)
                (lambda %admin (pair address mutez) (pair (list operation) (pair address mutez)))) ;
          storage (pair (address %owner) (mutez %price)) ;
          code { DUP ;
                 CDR ;
                 SWAP ;
                 CAR ;
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
                     IF { EXEC } { PUSH string "not admin" ; FAILWITH } } } }
        Initial storage: (Pair "tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm" 1)
        No delegate for this contract
        This origination was successfully applied
        Originated contracts:
          KT18rqmG2eV33cZYQR9JMDAtzf8yEp59Nnqm
        Storage size: 251 bytes
        Paid storage size diff: 251 bytes
        Consumed gas: 2483.822
        Balance updates:
          tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ... -ꜩ0.06275
          tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ... -ꜩ0.06425

New contract KT18rqmG2eV33cZYQR9JMDAtzf8yEp59Nnqm originated.
Contract memorized as LambdaAdminOracle3.
```

Ask:

```bash
~/C/m/lorentz-contract-vesting ❯❯❯ alpha-client --wait none transfer 0.000001 from $BOB_ADDRESS to KT18rqmG2eV33cZYQR9JMDAtzf8yEp59Nnqm \
  --entrypoint ask --arg 'Pair "KT1HHgfN2v47ogDikDLRKigaXaDAW2rSdyk9" "my_query"'
Waiting for the node to be bootstrapped before injection...
Current head: BLL1JWxNy3QR (timestamp: 2021-02-03T22:34:27-00:00, validation: 2021-02-03T22:34:35-00:00)
Node is bootstrapped, ready for injecting operations.
Estimated gas: 4050.339 units (will add 100 for safety)
Estimated storage: no bytes added
Operation successfully injected in the node.
Operation hash is 'ookvVazPMc8cDPPVmqLopdHWG5yWyFWnSTHKH8rv8ixGCnjyFrQ'
NOT waiting for the operation to be included.
Use command
  tezos-client wait for ookvVazPMc8cDPPVmqLopdHWG5yWyFWnSTHKH8rv8ixGCnjyFrQ to be included --confirmations 30 --branch BLL1JWxNy3QRSskj12b7fgLiutntgoruJjhEuCw3D5iYibYwfrc
and/or an external block explorer to make sure that it has been included.
This sequence of operations was run:
  Manager signed operations:
    From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
    Fee to the baker: ꜩ0.000731
    Expected counter: 526618
    Gas limit: 4151
    Storage limit: 0 bytes
    Balance updates:
      tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ............. -ꜩ0.000731
      fees(tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU,204) ... +ꜩ0.000731
    Transaction:
      Amount: ꜩ0.000001
      From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
      To: KT18rqmG2eV33cZYQR9JMDAtzf8yEp59Nnqm
      Entrypoint: ask
      Parameter: (Pair "KT1HHgfN2v47ogDikDLRKigaXaDAW2rSdyk9" "my_query")
      This transaction was successfully applied
      Updated storage:
        (Pair 0x0000aad02222472cdf9892a3011c01caf6407f027081 1)
      Storage size: 251 bytes
      Consumed gas: 4050.339
      Balance updates:
        tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ... -ꜩ0.000001
        KT18rqmG2eV33cZYQR9JMDAtzf8yEp59Nnqm ... +ꜩ0.000001
```

Make Fred the owner:

```bash
❯❯❯ alpha-client --wait none transfer 0 from $BOB_ADDRESS to KT18rqmG2eV33cZYQR9JMDAtzf8yEp59Nnqm \
  --entrypoint admin --arg '{ CDR; PUSH address "tz1RwoEdg4efDQHarsw6aKtMUYvg278Gv1ir"; PAIR; NIL operation; PAIR }'
Waiting for the node to be bootstrapped before injection...
Current head: BLXqbDdeAiFa (timestamp: 2021-02-03T22:45:47-00:00, validation: 2021-02-03T22:46:14-00:00)
Node is bootstrapped, ready for injecting operations.
Estimated gas: 3550.653 units (will add 100 for safety)
Estimated storage: no bytes added
Operation successfully injected in the node.
Operation hash is 'op1QEQKK2JuCf6MfDDMT76ZYiicWQ9AJiX6uF1z3TVu82xBd3mp'
NOT waiting for the operation to be included.
Use command
  tezos-client wait for op1QEQKK2JuCf6MfDDMT76ZYiicWQ9AJiX6uF1z3TVu82xBd3mp to be included --confirmations 30 --branch BLXqbDdeAiFa91V3B3mbqxt977SU9q673QpnahJtNQ9AjFjj9C5
and/or an external block explorer to make sure that it has been included.
This sequence of operations was run:
  Manager signed operations:
    From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
    Fee to the baker: ꜩ0.000687
    Expected counter: 526619
    Gas limit: 3651
    Storage limit: 0 bytes
    Balance updates:
      tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ............. -ꜩ0.000687
      fees(tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU,204) ... +ꜩ0.000687
    Transaction:
      Amount: ꜩ0
      From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
      To: KT18rqmG2eV33cZYQR9JMDAtzf8yEp59Nnqm
      Entrypoint: admin
      Parameter: { CDR ;
                   PUSH address "tz1RwoEdg4efDQHarsw6aKtMUYvg278Gv1ir" ;
                   PAIR ;
                   NIL operation ;
                   PAIR }
      This transaction was successfully applied
      Updated storage:
        (Pair 0x0000452d024b72d5897f14a02dc1a3b8e012c802cc3d 1)
      Storage size: 251 bytes
      Consumed gas: 3550.653
```

Set the price to `2 mutez`:

```bash
❯❯❯ alpha-client --wait none transfer 0 from $BOB_ADDRESS to KT18rqmG2eV33cZYQR9JMDAtzf8yEp59Nnqm \
  --entrypoint admin --arg '{ CAR; PUSH mutez 2; SWAP; PAIR; NIL operation; PAIR }'
Waiting for the node to be bootstrapped before injection...
Current head: BKvisxu111qX (timestamp: 2021-02-03T22:48:47-00:00, validation: 2021-02-03T22:49:05-00:00)
Node is bootstrapped, ready for injecting operations.
This simulation failed:
  Manager signed operations:
    From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
    Fee to the baker: ꜩ0
    Expected counter: 526620
    Gas limit: 1040000
    Storage limit: 60000 bytes
    Transaction:
      Amount: ꜩ0
      From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
      To: KT18rqmG2eV33cZYQR9JMDAtzf8yEp59Nnqm
      Entrypoint: admin
      Parameter: { CAR ; PUSH mutez 2 ; SWAP ; PAIR ; NIL operation ; PAIR }
      This operation FAILED.

Runtime error in contract KT18rqmG2eV33cZYQR9JMDAtzf8yEp59Nnqm:
  01: { parameter
  02:     (or (pair %ask (contract (pair string nat)) string)
  03:         (lambda %admin (pair address mutez) (pair (list operation) (pair address mutez)))) ;
  04:   storage (pair (address %owner) (mutez %price)) ;
  05:   code { DUP ;
  06:          CDR ;
  07:          SWAP ;
  08:          CAR ;
  09:          IF_LEFT
  10:            { DROP ;
  11:              DUP ;
  12:              CDR ;
  13:              AMOUNT ;
  14:              COMPARE ;
  15:              EQ ;
  16:              IF { NIL operation ; PAIR } { PUSH string "wrong price" ; FAILWITH } }
  17:            { SWAP ;
  18:              DUP ;
  19:              CAR ;
  20:              SENDER ;
  21:              COMPARE ;
  22:              EQ ;
  23:              IF { EXEC } { PUSH string "not admin" ; FAILWITH } } } }
At line 23 characters 53 to 61,
script reached FAILWITH instruction
with "not admin"
Fatal error:
  transfer simulation failed
```

But Bob is no longer the admin, so this fails:

```bash
❯❯❯ alpha-client --wait none transfer 0 from $BOB_ADDRESS to KT18rqmG2eV33cZYQR9JMDAtzf8yEp59Nnqm \
  --entrypoint admin --arg '{ CAR; PUSH mutez 2; SWAP; PAIR; NIL operation; PAIR }'
Waiting for the node to be bootstrapped before injection...
Current head: BKvisxu111qX (timestamp: 2021-02-03T22:48:47-00:00, validation: 2021-02-03T22:49:05-00:00)
Node is bootstrapped, ready for injecting operations.
This simulation failed:
  Manager signed operations:
    From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
    Fee to the baker: ꜩ0
    Expected counter: 526620
    Gas limit: 1040000
    Storage limit: 60000 bytes
    Transaction:
      Amount: ꜩ0
      From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
      To: KT18rqmG2eV33cZYQR9JMDAtzf8yEp59Nnqm
      Entrypoint: admin
      Parameter: { CAR ; PUSH mutez 2 ; SWAP ; PAIR ; NIL operation ; PAIR }
      This operation FAILED.

Runtime error in contract KT18rqmG2eV33cZYQR9JMDAtzf8yEp59Nnqm:
  01: { parameter
  02:     (or (pair %ask (contract (pair string nat)) string)
  03:         (lambda %admin (pair address mutez) (pair (list operation) (pair address mutez)))) ;
  04:   storage (pair (address %owner) (mutez %price)) ;
  05:   code { DUP ;
  06:          CDR ;
  07:          SWAP ;
  08:          CAR ;
  09:          IF_LEFT
  10:            { DROP ;
  11:              DUP ;
  12:              CDR ;
  13:              AMOUNT ;
  14:              COMPARE ;
  15:              EQ ;
  16:              IF { NIL operation ; PAIR } { PUSH string "wrong price" ; FAILWITH } }
  17:            { SWAP ;
  18:              DUP ;
  19:              CAR ;
  20:              SENDER ;
  21:              COMPARE ;
  22:              EQ ;
  23:              IF { EXEC } { PUSH string "not admin" ; FAILWITH } } } }
At line 23 characters 53 to 61,
script reached FAILWITH instruction
with "not admin"
Fatal error:
  transfer simulation failed
```

Fred can update it to `2 mutez`, however:

```bash
❯❯❯ alpha-client --wait none transfer 0 from $FRED_ADDRESS to KT18rqmG2eV33cZYQR9JMDAtzf8yEp59Nnqm \
  --entrypoint admin --arg '{ CAR; PUSH mutez 2; SWAP; PAIR; NIL operation; PAIR }'
Waiting for the node to be bootstrapped before injection...
Current head: BKwRQxtfx6oY (timestamp: 2021-02-03T22:51:37-00:00, validation: 2021-02-03T22:51:50-00:00)
Node is bootstrapped, ready for injecting operations.
Estimated gas: 3499.533 units (will add 100 for safety)
Estimated storage: no bytes added
Operation successfully injected in the node.
Operation hash is 'opQKw7eAnahYi6NFsgCMbqeU6RfQ89k9Y2R7BEvWwU4pAAnnZCr'
NOT waiting for the operation to be included.
Use command
  tezos-client wait for opQKw7eAnahYi6NFsgCMbqeU6RfQ89k9Y2R7BEvWwU4pAAnnZCr to be included --confirmations 30 --branch BKwRQxtfx6oYTdPqAQP4RAqqkAQfS4Ar15uj1xsHcnGLFmo6oLo
and/or an external block explorer to make sure that it has been included.
This sequence of operations was run:
  Manager signed operations:
    From: tz1RwoEdg4efDQHarsw6aKtMUYvg278Gv1ir
    Fee to the baker: ꜩ0.001259
    Expected counter: 1242109
    Gas limit: 10000
    Storage limit: 0 bytes
    Balance updates:
      tz1RwoEdg4efDQHarsw6aKtMUYvg278Gv1ir ............. -ꜩ0.001259
      fees(tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU,204) ... +ꜩ0.001259
    Revelation of manager public key:
      Contract: tz1RwoEdg4efDQHarsw6aKtMUYvg278Gv1ir
      Key: edpkvMCwX3MyDg92HckSwFVofR8hcZEjAqrhWJ8SGQgkGjgK1V1gPo
      This revelation was successfully applied
      Consumed gas: 1000
  Manager signed operations:
    From: tz1RwoEdg4efDQHarsw6aKtMUYvg278Gv1ir
    Fee to the baker: ꜩ0.000548
    Expected counter: 1242110
    Gas limit: 3600
    Storage limit: 0 bytes
    Balance updates:
      tz1RwoEdg4efDQHarsw6aKtMUYvg278Gv1ir ............. -ꜩ0.000548
      fees(tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU,204) ... +ꜩ0.000548
    Transaction:
      Amount: ꜩ0
      From: tz1RwoEdg4efDQHarsw6aKtMUYvg278Gv1ir
      To: KT18rqmG2eV33cZYQR9JMDAtzf8yEp59Nnqm
      Entrypoint: admin
      Parameter: { CAR ; PUSH mutez 2 ; SWAP ; PAIR ; NIL operation ; PAIR }
      This transaction was successfully applied
      Updated storage:
        (Pair 0x0000452d024b72d5897f14a02dc1a3b8e012c802cc3d 2)
      Storage size: 251 bytes
      Consumed gas: 3499.533
```

Bob now tries to ask with only `1 mutez`:

```bash
❯❯❯ alpha-client --wait none transfer 0.000001 from $BOB_ADDRESS to KT18rqmG2eV33cZYQR9JMDAtzf8yEp59Nnqm \
  --entrypoint ask --arg 'Pair "KT1HHgfN2v47ogDikDLRKigaXaDAW2rSdyk9" "my_query"'
Waiting for the node to be bootstrapped before injection...
Current head: BMAT8i5MGPt2 (timestamp: 2021-02-03T22:53:37-00:00, validation: 2021-02-03T22:53:53-00:00)
Node is bootstrapped, ready for injecting operations.
This simulation failed:
  Manager signed operations:
    From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
    Fee to the baker: ꜩ0
    Expected counter: 526620
    Gas limit: 1040000
    Storage limit: 60000 bytes
    Transaction:
      Amount: ꜩ0.000001
      From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
      To: KT18rqmG2eV33cZYQR9JMDAtzf8yEp59Nnqm
      Entrypoint: ask
      Parameter: (Pair "KT1HHgfN2v47ogDikDLRKigaXaDAW2rSdyk9" "my_query")
      This operation FAILED.

Runtime error in contract KT18rqmG2eV33cZYQR9JMDAtzf8yEp59Nnqm:
  01: { parameter
  02:     (or (pair %ask (contract (pair string nat)) string)
  03:         (lambda %admin (pair address mutez) (pair (list operation) (pair address mutez)))) ;
  04:   storage (pair (address %owner) (mutez %price)) ;
  05:   code { DUP ;
  06:          CDR ;
  07:          SWAP ;
  08:          CAR ;
  09:          IF_LEFT
  10:            { DROP ;
  11:              DUP ;
  12:              CDR ;
  13:              AMOUNT ;
  14:              COMPARE ;
  15:              EQ ;
  16:              IF { NIL operation ; PAIR } { PUSH string "wrong price" ; FAILWITH } }
  17:            { SWAP ;
  18:              DUP ;
  19:              CAR ;
  20:              SENDER ;
  21:              COMPARE ;
  22:              EQ ;
  23:              IF { EXEC } { PUSH string "not admin" ; FAILWITH } } } }
At line 16 characters 71 to 79,
script reached FAILWITH instruction
with "wrong price"
Fatal error:
  transfer simulation failed
```

But it fails, since the price has been updated to `2 mutez`.

Bob then tries to ask with `2 mutez`:

```bash
❯❯❯ alpha-client --wait none transfer 0.000002 from $BOB_ADDRESS to KT18rqmG2eV33cZYQR9JMDAtzf8yEp59Nnqm \
  --entrypoint ask --arg 'Pair "KT1HHgfN2v47ogDikDLRKigaXaDAW2rSdyk9" "my_query"'
Waiting for the node to be bootstrapped before injection...
Current head: BLTJP9yYX7aP (timestamp: 2021-02-03T22:54:37-00:00, validation: 2021-02-03T22:54:58-00:00)
Node is bootstrapped, ready for injecting operations.
Estimated gas: 4050.339 units (will add 100 for safety)
Estimated storage: no bytes added
Operation successfully injected in the node.
Operation hash is 'opMaxM1iqD8SbzYd9URMPnXG8jZDXRUDSo3mrYH4K5g52tui9yX'
NOT waiting for the operation to be included.
Use command
  tezos-client wait for opMaxM1iqD8SbzYd9URMPnXG8jZDXRUDSo3mrYH4K5g52tui9yX to be included --confirmations 30 --branch BLTJP9yYX7aPea6unhsvzJwKYRvUZuh7Wpqjx6GAJmgoLyingDM
and/or an external block explorer to make sure that it has been included.
This sequence of operations was run:
  Manager signed operations:
    From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
    Fee to the baker: ꜩ0.000731
    Expected counter: 526620
    Gas limit: 4151
    Storage limit: 0 bytes
    Balance updates:
      tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ............. -ꜩ0.000731
      fees(tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU,204) ... +ꜩ0.000731
    Transaction:
      Amount: ꜩ0.000002
      From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
      To: KT18rqmG2eV33cZYQR9JMDAtzf8yEp59Nnqm
      Entrypoint: ask
      Parameter: (Pair "KT1HHgfN2v47ogDikDLRKigaXaDAW2rSdyk9" "my_query")
      This transaction was successfully applied
      Updated storage:
        (Pair 0x0000452d024b72d5897f14a02dc1a3b8e012c802cc3d 2)
      Storage size: 251 bytes
      Consumed gas: 4050.339
      Balance updates:
        tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ... -ꜩ0.000002
        KT18rqmG2eV33cZYQR9JMDAtzf8yEp59Nnqm ... +ꜩ0.000002
```

And it succeeds.

We can now verify that the oracle contract holds `3 mutez`:

```bash
❯❯❯ alpha-client get balance for KT18rqmG2eV33cZYQR9JMDAtzf8yEp59Nnqm

0.000003 ꜩ
```

Fred decides to send the entire balance to Bob:

```bash
❯❯❯ alpha-client --wait none transfer 0 from $FRED_ADDRESS to KT18rqmG2eV33cZYQR9JMDAtzf8yEp59Nnqm \
  --entrypoint admin --arg "{ NIL operation; PUSH address \"$BOB_ADDRESS\"; CONTRACT unit; IF_NONE { PUSH string \"not receiver\";FAILWITH } { PUSH mutez 3;UNIT;TRANSFER_TOKENS;CONS;PAIR } }"

Waiting for the node to be bootstrapped before injection...
Current head: BMS2Vkyw5Bgq (timestamp: 2021-02-03T23:00:08-00:00, validation: 2021-02-03T23:00:23-00:00)
Node is bootstrapped, ready for injecting operations.
Estimated gas: 5414.383 units (will add 100 for safety)
Estimated storage: no bytes added
Operation successfully injected in the node.
Operation hash is 'onhhCv9Fshn73KbDosiYRnZvov29xCcYmNduzE4hCtNzU8awszW'
NOT waiting for the operation to be included.
Use command
  tezos-client wait for onhhCv9Fshn73KbDosiYRnZvov29xCcYmNduzE4hCtNzU8awszW to be included --confirmations 30 --branch BMS2Vkyw5BgqpdHnp2SNwmrR8LSukTh7fgiVsisSTAW8zorsh38
and/or an external block explorer to make sure that it has been included.
This sequence of operations was run:
  Manager signed operations:
    From: tz1RwoEdg4efDQHarsw6aKtMUYvg278Gv1ir
    Fee to the baker: ꜩ0.00092
    Expected counter: 1242111
    Gas limit: 5515
    Storage limit: 0 bytes
    Balance updates:
      tz1RwoEdg4efDQHarsw6aKtMUYvg278Gv1ir ............. -ꜩ0.00092
      fees(tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU,204) ... +ꜩ0.00092
    Transaction:
      Amount: ꜩ0
      From: tz1RwoEdg4efDQHarsw6aKtMUYvg278Gv1ir
      To: KT18rqmG2eV33cZYQR9JMDAtzf8yEp59Nnqm
      Entrypoint: admin
      Parameter: { NIL operation ;
                   PUSH address "tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm" ;
                   CONTRACT unit ;
                   IF_NONE
                     { PUSH string "not receiver" ; FAILWITH }
                     { PUSH mutez 3 ; UNIT ; TRANSFER_TOKENS ; CONS ; PAIR } }
      This transaction was successfully applied
      Updated storage:
        (Pair 0x0000452d024b72d5897f14a02dc1a3b8e012c802cc3d 2)
      Storage size: 251 bytes
      Consumed gas: 3987.383
    Internal operations:
      Transaction:
        Amount: ꜩ0.000003
        From: KT18rqmG2eV33cZYQR9JMDAtzf8yEp59Nnqm
        To: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
        This transaction was successfully applied
        Consumed gas: 1427
        Balance updates:
          KT18rqmG2eV33cZYQR9JMDAtzf8yEp59Nnqm ... -ꜩ0.000003
          tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ... +ꜩ0.000003
```

