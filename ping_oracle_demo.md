

```bash
❯❯❯ tezos-client --wait none originate contract OraclePinger \
  transferring 0 from tz1QS8VYYVDjv7iReBzXeheL6x63A1oATTj8 running \
  "$(cat ping_oracle.tz | tr -d '\n')" \
  --init "(Pair True (Pair \"no_entry\" 0))" --burn-cap 1

Waiting for the node to be bootstrapped...
Current head: BMXK8i1Rhk8s (timestamp: 2021-03-26T17:47:23.000-00:00, validation: 2021-03-26T17:47:50.485-00:00)
Node is bootstrapped.
Estimated gas: 3849.671 units (will add 100 for safety)
Estimated storage: 933 bytes added (will add 20 for safety)
Operation successfully injected in the node.
Operation hash is 'ooR41PHAsUoU7kkNLn73Go6rpRSCEtPgoR9hLuk6eJt33J4rBYa'
NOT waiting for the operation to be included.
Use command
  tezos-client wait for ooR41PHAsUoU7kkNLn73Go6rpRSCEtPgoR9hLuk6eJt33J4rBYa to be included --confirmations 30 --branch BMXK8i1Rhk8sTAB8hrD2ugoCZtxNjcWNQXKD2uHhLNjkfjR4X4G
and/or an external block explorer to make sure that it has been included.
This sequence of operations was run:
  Manager signed operations:
    From: tz1QS8VYYVDjv7iReBzXeheL6x63A1oATTj8
    Fee to the baker: ꜩ0.0013
    Expected counter: 323902
    Gas limit: 3950
    Storage limit: 953 bytes
    Balance updates:
      tz1QS8VYYVDjv7iReBzXeheL6x63A1oATTj8 ................. -ꜩ0.0013
      fees(the baker who will include this operation,58) ... +ꜩ0.0013
    Origination:
      From: tz1QS8VYYVDjv7iReBzXeheL6x63A1oATTj8
      Credit: ꜩ0
      Script:
        { parameter (or (unit %ask) (pair %respond string nat)) ;
          storage (pair (bool %waiting) (pair (string %response) (nat %counter))) ;
          code { DUP ;
                 CAR ;
                 DIP { CDR } ;
                 IF_LEFT
                   { DROP ;
                     DUP ;
                     CAR ;
                     DIP { CDR } ;
                     IF { PUSH string "waiting" ; FAILWITH }
                        { SENDER ;
                          PUSH address "tz1QS8VYYVDjv7iReBzXeheL6x63A1oATTj8" ;
                          COMPARE ;
                          EQ ;
                          IF { PUSH address "tz1QS8VYYVDjv7iReBzXeheL6x63A1oATTj8" ;
                               CONTRACT (pair (contract (pair string nat)) string) ;
                               IF_NONE
                                 { PUSH string "oracle not found" ; FAILWITH }
                                 { SWAP ;
                                   DIP { PUSH string "some_query" ;
                                         SELF %respond ;
                                         PAIR ;
                                         PUSH mutez 10 ;
                                         SWAP ;
                                         TRANSFER_TOKENS } ;
                                   PUSH bool True ;
                                   PAIR ;
                                   SWAP ;
                                   NIL operation ;
                                   SWAP ;
                                   CONS } }
                             { PUSH string "not owner" ; FAILWITH } } }
                   { SWAP ;
                     DUP ;
                     CAR ;
                     DIP { CDR } ;
                     IF { SENDER ;
                          PUSH address "tz1QS8VYYVDjv7iReBzXeheL6x63A1oATTj8" ;
                          COMPARE ;
                          EQ ;
                          IF { CDR ;
                               SWAP ;
                               DUP ;
                               CAR ;
                               DIP { CDR ;
                                     DUP ;
                                     PUSH nat 1 ;
                                     ADD ;
                                     DIP { COMPARE ; EQ ; IF {} { PUSH string "unexpected counter" ; FAILWITH } } } ;
                               PAIR ;
                               PUSH bool False ;
                               PAIR ;
                               NIL operation }
                             { PUSH string "not responder" ; FAILWITH } }
                        { PUSH string "not waiting" ; FAILWITH } } ;
                 PAIR } }
        Initial storage: (Pair True (Pair "no_entry" 0))
        No delegate for this contract
        This origination was successfully applied
        Originated contracts:
          KT1BzzvbYCsBHboEs23dCkCQLB78G8hX9noY
        Storage size: 676 bytes
        Paid storage size diff: 676 bytes
        Consumed gas: 3849.671
        Balance updates:
          tz1QS8VYYVDjv7iReBzXeheL6x63A1oATTj8 ... -ꜩ0.169
          tz1QS8VYYVDjv7iReBzXeheL6x63A1oATTj8 ... -ꜩ0.06425

New contract KT1BzzvbYCsBHboEs23dCkCQLB78G8hX9noY originated.
Contract memorized as OraclePinger.
```

```bash
❯❯❯ tezos-client --wait none transfer 0 from tz1QS8VYYVDjv7iReBzXeheL6x63A1oATTj8 to KT1BzzvbYCsBHboEs23dCkCQLB78G8hX9noY \
  --entrypoint respond --arg 'Pair "new_storage" 0' --burn-cap 0.00075

Waiting for the node to be bootstrapped...
Current head: BKjrwMjN412K (timestamp: 2021-03-26T17:48:23.000-00:00, validation: 2021-03-26T17:48:46.945-00:00)
Node is bootstrapped.
Estimated gas: 4740.998 units (will add 100 for safety)
Estimated storage: 3 bytes added (will add 20 for safety)
Operation successfully injected in the node.
Operation hash is 'onkxbwHxpUrAajg7DZjWJ2AuJXFQ1whFowEUNzL7d8nB518NgmT'
NOT waiting for the operation to be included.
Use command
  tezos-client wait for onkxbwHxpUrAajg7DZjWJ2AuJXFQ1whFowEUNzL7d8nB518NgmT to be included --confirmations 30 --branch BKjrwMjN412KQvqRYUu7TxPfGv39vFUg9udvxNFvpSytToNjxec
and/or an external block explorer to make sure that it has been included.
This sequence of operations was run:
  Manager signed operations:
    From: tz1QS8VYYVDjv7iReBzXeheL6x63A1oATTj8
    Fee to the baker: ꜩ0.000768
    Expected counter: 323903
    Gas limit: 4841
    Storage limit: 23 bytes
    Balance updates:
      tz1QS8VYYVDjv7iReBzXeheL6x63A1oATTj8 ................. -ꜩ0.000768
      fees(the baker who will include this operation,58) ... +ꜩ0.000768
    Transaction:
      Amount: ꜩ0
      From: tz1QS8VYYVDjv7iReBzXeheL6x63A1oATTj8
      To: KT1BzzvbYCsBHboEs23dCkCQLB78G8hX9noY
      Entrypoint: respond
      Parameter: (Pair "new_storage" 0)
      This transaction was successfully applied
      Updated storage: (Pair False (Pair "new_storage" 1))
      Storage size: 679 bytes
      Paid storage size diff: 3 bytes
      Consumed gas: 4740.998
      Balance updates:
        tz1QS8VYYVDjv7iReBzXeheL6x63A1oATTj8 ... -ꜩ0.00075
```

