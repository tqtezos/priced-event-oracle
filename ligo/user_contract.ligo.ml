
let oracle_address : address =
  ("KT1VcDH6SWd3QAWZf3M1DuD4i7zqtZq8CXSa%ask" : address)

let responder_address : address =
  ("tz1drDkC2hJk1pzyMuqbDpvB3HeRVmVJRL2E" : address)

let query_string : string =
  "query=AirTemperatureData[Entity[\"City\", {\"NewYork\", \"NewYork\", \"UnitedStates\"}]]&precision=auto&unit=metric"

let oracle_price : tez = 1mutez


type response = bytes * nat

let self_response_ref : response contract = Tezos.self "%respond"

type parameter_type =
    Ask
  | Respond of response

type storage_type =
{
  owner : address;
  waiting : bool;
  response : bytes;
  counter : nat;
}

type return = operation list * storage_type

let ask (_u : unit) : operation =
  match (Tezos.get_contract_opt (oracle_address): ((response contract * string) contract) option) with
    Some contract_ref ->
      Tezos.transaction (self_response_ref, query_string) oracle_price contract_ref
  | None -> (failwith ("oracle not found") : operation)

let main (parameter, storage : parameter_type * storage_type) : return =
  match parameter with
  | Ask ->
       if (Tezos.sender = storage.owner) then
        ([ask ()],
        { storage with waiting = true })
       else
         (failwith "only admin may update" : return)

  | Respond res ->
      if storage.waiting
      then if (Tezos.sender = responder_address)
      then
        let (response_bytes, response_counter) = res in
        if (response_counter = storage.counter)
        then 
          (([] : operation list),
            { storage with
                waiting = false;
                response = response_bytes;
                counter = storage.counter + 1n
            })
      else (failwith "unexpected counter" : return)
      else (failwith "not responder" : return)
      else (failwith "not waiting" : return)

