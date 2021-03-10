
```bash
❯❯❯ alpha-client --wait none originate contract LambdaAdminOracleSmaller \
  transferring 0 from $BOB_ADDRESS running \
  "$(cat lambda_admin_oracle_smaller.tz | tr -d '\n')" \
  --init "(Pair \"$BOB_ADDRESS\" 1)" \
  --burn-cap 0.1115
Waiting for the node to be bootstrapped before injection...
Current head: BLPgEsvrWuVT (timestamp: 2021-02-18T18:19:58-00:00, validation: 2021-02-18T18:20:20-00:00)
Node is bootstrapped, ready for injecting operations.
Estimated gas: 2375.026 units (will add 100 for safety)
Estimated storage: 446 bytes added (will add 20 for safety)
Operation successfully injected in the node.
Operation hash is 'oorrYq4aVTA1ji2MyUSKUcj1s96kaRd3VWVB3XUxHeinhFBcrDx'
NOT waiting for the operation to be included.
Use command
  tezos-client wait for oorrYq4aVTA1ji2MyUSKUcj1s96kaRd3VWVB3XUxHeinhFBcrDx to be included --confirmations 30 --branch BLPgEsvrWuVTE31GuiDVmyu4tKMCXbBMehi18ap2FEFv2QZS1q5
and/or an external block explorer to make sure that it has been included.
This sequence of operations was run:
  Manager signed operations:
    From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
    Fee to the baker: ꜩ0.00068
    Expected counter: 526621
    Gas limit: 2476
    Storage limit: 466 bytes
    Balance updates:
      tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ............. -ꜩ0.00068
      fees(tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU,224) ... +ꜩ0.00068
    Origination:
      From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
      Credit: ꜩ0
      Script:
        { parameter
            (or (pair %ask (contract (pair string nat)) string)
                (lambda %u (pair address mutez) (pair (list operation) (pair address mutez)))) ;
          storage (pair address mutez) ;
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
                     IF { NIL operation ; PAIR } { FAILWITH } }
                   { SWAP ;
                     DUP ;
                     CAR ;
                     SENDER ;
                     COMPARE ;
                     EQ ;
                     IF { EXEC } { FAILWITH } } } }
        Initial storage: (Pair "tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm" 1)
        No delegate for this contract
        This origination was successfully applied
        Originated contracts:
          KT1XXUxCs5DCuYf7d5t9HXMTiDzRz6ANjDSL
        Storage size: 189 bytes
        Paid storage size diff: 189 bytes
        Consumed gas: 2375.026
        Balance updates:
          tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ... -ꜩ0.04725
          tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ... -ꜩ0.06425

New contract KT1XXUxCs5DCuYf7d5t9HXMTiDzRz6ANjDSL originated.
Contract memorized as LambdaAdminOracleSmaller.
```


