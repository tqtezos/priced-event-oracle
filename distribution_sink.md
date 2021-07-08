
~/C/m/priced-event-oracle ❯❯❯ tezos-client --wait none originate contract DistributionSink \                                                                                                                                                                 $ 
  transferring 0 from tz1QS8VYYVDjv7iReBzXeheL6x63A1oATTj8 running \
  "$(cat distribution_sink.tz | tr -d '\n')" --burn-cap 1
Waiting for the node to be bootstrapped...
Current head: BLBSxwMhYe8H (timestamp: 2021-03-25T20:41:07.000-00:00, validation: 2021-03-25T20:41:09.974-00:00)
Node is bootstrapped.
Estimated gas: 1620.274 units (will add 100 for safety)
Estimated storage: 303 bytes added (will add 20 for safety)
Operation successfully injected in the node.
Operation hash is 'ooxkNGaiBYxPPfairb8mf2q71zwhGDnQ7Kb5A8gUek6z8uUDPYE'
NOT waiting for the operation to be included.
Use command
  tezos-client wait for ooxkNGaiBYxPPfairb8mf2q71zwhGDnQ7Kb5A8gUek6z8uUDPYE to be included --confirmations 30 --branch BLBSxwMhYe8HvBgGzoRoYUwk3ASrjgyCoFQNtqQ8784QB6xUfkJ
and/or an external block explorer to make sure that it has been included.
This sequence of operations was run:
  Manager signed operations:
    From: tz1QS8VYYVDjv7iReBzXeheL6x63A1oATTj8
    Fee to the baker: ꜩ0.000359
    Expected counter: 323895
    Gas limit: 1000
    Storage limit: 0 bytes
    Balance updates:
      tz1QS8VYYVDjv7iReBzXeheL6x63A1oATTj8 ................. -ꜩ0.000359
      fees(the baker who will include this operation,56) ... +ꜩ0.000359
    Revelation of manager public key:
      Contract: tz1QS8VYYVDjv7iReBzXeheL6x63A1oATTj8
      Key: edpkukGE7x3P4U3VR3rN59yAkvj7LmMtq42HcVsrGngz6zLoFgvZxj
      This revelation was successfully applied
      Consumed gas: 1000
  Manager signed operations:
    From: tz1QS8VYYVDjv7iReBzXeheL6x63A1oATTj8
    Fee to the baker: ꜩ0.000352
    Expected counter: 323896
    Gas limit: 1721
    Storage limit: 323 bytes
    Balance updates:
      tz1QS8VYYVDjv7iReBzXeheL6x63A1oATTj8 ................. -ꜩ0.000352
      fees(the baker who will include this operation,56) ... +ꜩ0.000352
    Origination:
      From: tz1QS8VYYVDjv7iReBzXeheL6x63A1oATTj8
      Credit: ꜩ0
      Script:
        { parameter (list (pair nat (list address))) ;
          storage unit ;
          code { CDR ; NIL operation ; PAIR } }
        Initial storage: Unit
        No delegate for this contract
        This origination was successfully applied
        Originated contracts:
          KT1B9seWkK5o5kyuSLNDPWMbKXNKq7y3aZvc
        Storage size: 46 bytes
        Paid storage size diff: 46 bytes
        Consumed gas: 1620.274
        Balance updates:
          tz1QS8VYYVDjv7iReBzXeheL6x63A1oATTj8 ... -ꜩ0.0115
          tz1QS8VYYVDjv7iReBzXeheL6x63A1oATTj8 ... -ꜩ0.06425

New contract KT1B9seWkK5o5kyuSLNDPWMbKXNKq7y3aZvc originated.
Contract memorized as DistributionSink.


