class StringIdMaker(object):
    def __init__(self):
        self.id_map = {}
        self.next_id = 0
    
    """
    Input: string
    Output: an int id

    Don't save this out anywhere, it can change depending on the order of calls to it
    They are guaranteed within one session to have a 1-to-1 correspondence with input
    strings.
    """
    def make_id(self, str):
        if str not in self.id_map:
            self.id_map[str] = self.next_id
            self.next_id += 1

        return self.id_map[str]


