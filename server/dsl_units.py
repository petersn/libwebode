
import collections

class Units:
    def __init__(self, parent, scalar, base_units):
        self.parent = parent
        self.scalar = scalar
        self.base_units = base_units

    def add(self, other):
        pass

    def mult(self, other):
        pass

    def exp(self, pow: int):
        pass

class UnitSystem:
    def __init__(self, failure_function):
        self.failure_function = failure_function
        self.prefixes = {}
        self.base_units = set()
        self.defined_units = {}

    def add_base_unit(self, name):
        if name in self.base_units:
            self.failure_function("Redefinition of base unit: %s" % (name,))
        self.base_units.add(name)

    def add_defined_units(self, name, definition):
        if name in self.defined_units:
            self.failure_function("Redefinition of unit: %s" % (name,))
        self.defined_units[name] = definition

    def add_prefix(self, prefix, value):
        if prefix in self.defined_prefixes:
            self.failure_function("")
        self.prefixes[prefix] = value

    def parse_unit_name(self, name):
        pass

    def format_units(self, units):
        # Greedily take the step that reduces our magnitude maximally.
        output = collections.defaultdict(int)
        state = {k: units.base_units.get(k, 0) for k in self.base_units}
        #for 

    def sanity_check(self):
        # We do this after defining each new unit, so technically defining n units
        # can take cubic time. Whatever, who cares, it'll be plenty fast in practice.
        possible_units = set()
        for prefix in list(self.prefixes) + [""]:
            for suffix in list(self.base_units) + list(self.defined_units) + [""]:
                combo = prefix + suffix
                if combo in possible_units:
                    self.failure_function("Unit system allows for ambiguous unit named: %s" % combo)
                possible_units.add(combo)
