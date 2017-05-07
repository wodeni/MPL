import sys
import struct
import pprint

class RunLengthEncodedParser:
    """
    Parser for Run Length Encode (RLE) strings / files.
    More information: http://www.conwaylife.com/w/index.php?title=Run_Length_Encoded
    """
    def __init__(self, rle_string):
        self.rle_string = rle_string
        self.name = ""
        self.comments = []
        self.author = ""
        self.size_x = 0
        self.size_y = 0
        self.rule_birth = []
        self.rule_survival = []
        self.pattern_raw = ""
        # Fill in instance attributes by parsing the raw strings
        self.populate_attributes(self.rle_string.strip().splitlines())
        self.pattern_2d_array = self.populate_pattern(self.pattern_raw, self.size_x, self.size_y)

    def populate_attributes(self, lines):
        """
        This method performs all the string parsing required to parse the various
        fields of data into their respective data members.
        """
        for line in lines:
            # Name of the pattern
            if line.startswith("#N"):
                self.name = line.lstrip("#N ")
            # Comments accompanying the pattern
            elif line.startswith("#C") or line.startswith("#c"):
                self.comments.append(line.lstrip("#Cc "))
            # Authorship of the pattern
            elif line.startswith("#O"):
                self.author = line.lstrip("#O ")
            # Grid sizes and rules
            elif line.startswith("x"):
                data = line.split(",")
                for d in data:
                    # Grid sizes
                    if d.strip().startswith("x"):
                        _, x = d.split("=")
                        self.size_x = int(x.strip())
                    elif d.strip().startswith("y"):
                        _, y = d.split("=")
                        self.size_y = int(y.strip())
                    # Rules
                    elif d.strip().startswith("rule"):
                        _, rule = d.split("=")
                        for r in rule.strip().split("/"):
                            if r.startswith("B"):
                                for digit in list(r.lstrip("B")):
                                    self.rule_birth.append(int(digit))
                            if r.startswith("S"):
                                for digit in list(r.lstrip("S")):
                                    self.rule_survival.append(int(digit))
            # Other lines should contain the actual pattern
            else:
                self.pattern_raw += line.strip(" \n\r\t")

    def populate_pattern(self, pattern_raw, size_x, size_y, default_cell='b'):
        pattern = []
        pattern_rows = pattern_raw.rstrip("!").split("$")
        assert len(pattern_rows) == size_y, \
        "Number of data rows {0} does not match size y = {1}".format(len(pattern_rows), size_y)
        for y in range(size_y):
            pattern.append([])
            tmp_num_str = ""
            for c in pattern_rows[y]:
                if self.isdigit(c):
                    tmp_num_str += c
                else:
                    if tmp_num_str == "":
                        num_cells = 1
                    else:
                        num_cells = int(tmp_num_str)
                    for n in range(num_cells):
                        pattern[y].append(c)
                    #reset count until another number is encountered
                    tmp_num_str = ""
            #fill in empty spaces at end of each row
            for _ in range(len(pattern[y]), size_x):
                pattern[y].append(default_cell)
        return pattern


    def isdigit(self, c):
        """Returns True is the character is a digit"""
        return '0' <= c <= '9'


    def __str__(self):
        return self.rle_string

    # Getters
    def get_name(self):
        return self.name

    def get_comments(self):
        return self.comments

    def get_author(self):
        return self.author

    def get_size_x(self):
        return self.size_x

    def get_size_y(self):
        return self.size_y

    def get_rule_birth(self):
        return self.rule_birth

    def get_rule_survival(self):
        return self.rule_survival

    def get_pattern_raw(self):
        return self.pattern_raw

    def get_pattern_2d_array(self):
        return self.pattern_2d_array

    def get_human_friendly_pattern(self):
        pattern_str = ""
        for row in self.pattern_2d_array:
            row_str = ""
            for c in row:
                if c == 'b':
                    row_str += '.'
                else:
                    row_str += c
            pattern_str += row_str + '\n'
        return pattern_str

def output_rle(filename, raw_arr, nrow, ncol, paddle):
    with open(filename, "wb") as fp:
        for i in range(0, paddle):
            for i in range(0, ncol):
                fp.write(struct.pack('i', 0))
        for row in raw_arr:
            for i in range(0, paddle):
                fp.write(struct.pack('i', 0))
            for e in row:
                if(e == 'b'):
                    res = 0
                else:
                    res = 1
                fp.write(struct.pack('i', res))
            # Fill the rest of the row
            for i in range(0, ncol - len(row) - paddle):
                fp.write(struct.pack('i', 0))

        # Fill the rest of the board
        for i in range(0, nrow - len(raw_arr) - paddle):
            for i in range(0, ncol):
                fp.write(struct.pack('i', 0))


def main(argv):
    sample_rle = \
"""#N Gosper glider gun
#C This was the first gun discovered.
#C As its name suggests, it was discovered by Bill Gosper.
#O Bill Gosper Nov. 1970
x = 36, y = 9, rule = B3/S23
24bo$22bobo$12b2o6b2o12b2o$11bo3bo4b2o12b2o$2o8bo5bo3b2o$2o8bo3bob2o4b
obo$10bo5bo7bo$11bo3bo$12b2o! """

    if(len(argv) !=  5):
        print("usage: <input filename> <output filename> <width> <height>")
        return
    sample_rle = open(argv[1], 'r').read()
    width = int(argv[3])
    height = int(argv[4])

    rle_parser = RunLengthEncodedParser(sample_rle)
    print("name:", rle_parser.get_name())
    print("comments:")
    pprint.pprint(rle_parser.get_comments())
    print("author:", rle_parser.get_author())
    print("size_x:", rle_parser.get_size_x())
    print("size_y:", rle_parser.get_size_y())
    print("rule_birth:", rle_parser.get_rule_birth())
    print("rule_survival:", rle_parser.get_rule_survival())
    print("pattern_raw:", rle_parser.get_pattern_raw())
    print("pattern_2d_array:")
    print(rle_parser.get_pattern_2d_array())
    print("human_friendly_pattern:")
    print(rle_parser.get_human_friendly_pattern())

    raw_arr = rle_parser.get_pattern_2d_array()
    print("writing to: " + argv[2])
    # output_rle(argv[1], raw_arr, 100, 100, 30)
    output_rle(argv[2], raw_arr, width, height, 6)

if(__name__ == "__main__"):
    print (sys.argv)
    main(sys.argv)
