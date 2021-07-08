
## Example use case

Suppose we wanted to create a smart contract to insure against
flooding.

Furthermore, suppose an instance of this oracle exists that
accepts physical locations and returns a `string`: `FLOODED` or `NOT_FLOODED`.

Then our insurance contract can take the following form:

Storage:
- `oracle_address : address`: The `address` of the priced event oracle
- `oracle_price : mutez`: The `price` of the priced event oracle
- `premium : mutez`: The cost per `period` to be insured
- `payout : mutez`: The amount paid out in case of a flood
- `period : nat`: The amount of time (in seconds) that you're covered for per payment of the `premium`
- `policyholders : big_map address (pair (pait timestamp (bool %paid_out)) string)`
  + `address`: The policyholder
  + `timestamp`: The last time the `premium` was paid
  + `bool`: Has this policyholder already made a claim this `period`?
  + `string`: The policyholder's location
- `claim_counter : nat`: The number of claims already made
- `current_claim : option (pair address string)`:
  The `address` and location `string` of the policyholder currently making a claim

Parameters:
- `(string %pay_premium)`:
  + The `string` is the policyholder's location: it will be updated if it already exists
  + Accept only the exact `premium` amount in Tez (or fail)
  + Updates the `timestamp` for the `policyholder` to `NOW`
  + Resets the `bool` to `false` (there hasn't been a claim yet)
  + Fails if the contract has insufficient balance to pay out a claim
- `(unit %make_claim)`
  + Fails if the `SENDER` `address` is not a known `policyholder`
  + Fails if a claim is already being made
  + Fails if the policyholder has not paid the premium within `period` seconds
    of the `timestamp`
  + Fails if the policyholder has already successfully made a claim this `period`, i.e. if the `paid_out` `bool` is `true`
  + Fails if the `oracle_price` in Tez is not sent by the user
  + Sets the `current_claim` to the `address` and location `string` of the
    policyholder currently making a claim
  + Calls the _priced event oracle_ with the policyholder's location `string` and
    the `confirm_claim` entrypoint of this contract and pays the `oracle_price`
- `(pair %confirm_claim string nat)`
  + Fails if a `current_claim` is `None`, i.e. we are not expecting a confirmation from the oracle
  + Fails if the `SENDER` is not the stored `oracle_address`
  + Fails if the `nat` is not the current `claim_counter`
  + Increments the `claim_counter` by one
  + If the `string` equals `FLOODED`, it sends the `payout` amount to the policyholder's `address` and
    the policyholder's `paid_out` `bool` to `true`
  + Sets the `current_claim` to `None`

### User flow

1. A user pays the `premium` for first time by sending their location `string`
  and the correct amount of Tez to the contract
2. The user makes a claim by calling the `make_claim` entrypoint with the fee for the oracle
3. The insurance contract calls the priced event oracle's `ask` entrypoint with the user's location `string`
4. The priced event oracle consumes the `query` and the oracle's host calculates the result `string`: `FLOODED` or `NOT_FLOODED`
5. The oracle's host calls `confirm_claim` with the result `string` and `counter`: the user is paid if its `FLOODED`


