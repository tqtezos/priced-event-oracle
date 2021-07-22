import smartpy as sp

oracle_address = sp.address("KT1VcDH6SWd3QAWZf3M1DuD4i7zqtZq8CXSa%ask")
responder_address = sp.address("tz1drDkC2hJk1pzyMuqbDpvB3HeRVmVJRL2E")

query_string = sp.string("query=AirTemperatureData[Entity[\\\"City\\\", {\\\"NewYork\\\", \\\"NewYork\\\", \\\"UnitedStates\\\"}]]&precision=auto&unit=metric")
# query_string = sp.string('\\"')

oracle_price = sp.mutez(1)

response = sp.TPair(sp.TBytes, sp.TNat)

self_response_ref = sp.self_entry_point(entry_point = 'respond')

storage_type = sp.TRecord(
    owner = sp.TAddress,
    waiting = sp.TBool,
    response = sp.TBytes,
    counter = sp.TNat
)

return_t = sp.TPair(sp.TList(sp.TOperation), storage_type)

class OracleUserContract(sp.Contract):
    def __init__(self, **kargs):
        self.init_type(storage_type)
        self.init(**kargs)

    @sp.entry_point
    def ask(self, params):
        sp.set_type(params, sp.TUnit)
        sp.verify(sp.sender == self.data.owner, "only admin may update")
        self.data.waiting = sp.bool(True)
        contract_ref = sp.contract(
            sp.TPair(sp.TContract(response), sp.TString),
            oracle_address).open_some("oracle not found")
        sp.transfer(sp.pair(self_response_ref, query_string), oracle_price, contract_ref)

    @sp.entry_point
    def respond(self, res):
        sp.set_type(res, response)
        sp.verify(self.data.waiting, "not waiting")
        sp.verify(sp.sender == responder_address, "not responder")

        response_bytes, response_counter = sp.match_pair(res)
        sp.verify(response_counter == self.data.counter, "unexpected counter")

        self.data.waiting = sp.bool(False)
        self.data.response = response_bytes
        self.data.counter += 1

example_storage = sp.record(
    owner = responder_address,
    waiting = sp.bool(False),
    response = sp.bytes('0x'),
    counter = sp.nat(0)
)

@sp.add_test(name = "Wrong sender ask")
def ask_wrong_sender():
    scenario = sp.test_scenario()
    scenario.h1("Wrong sender ask")
    c1 = OracleUserContract(arg = example_storage)
    scenario += c1
    c1.ask().run(valid = False, sender = oracle_address)

@sp.add_test(name = "Owner ask")
def ask_test():
    scenario = sp.test_scenario()
    scenario.h1("Owner ask")
    c1 = OracleUserContract(arg = example_storage)
    scenario += c1
    c1.set_initial_balance(sp.tez(10))
    c1.ask().run(sender = responder_address, amount = sp.mutez(1))

# A contract with a simple storage
sp.add_compilation_target("example_with_storage", OracleUserContract(arg = example_storage))

