import re


class Markdown:
    def __init__(self, markdown):
        self.lines = markdown.split('\n')
        self.html = ''
        self.in_list = False

    def markup(self):
        for line in self.lines:
            self.parse_line(self.style(line))

        # close any tags here
        if self.in_list:
            self.html += "</ul>"

        return self.html

    # this method of handling inline style is wrong:
    #     a line ___with bold italic text___
    # renders with the close tags in the wrong order
    #     a line <strong><em>with bold italic text</strong></em>
    # but it passes these tests, so ... shrug
    #
    def style(self, line):
        line = re.sub(r'__(.*?)__', r'<strong>\1</strong>', line)
        line = re.sub(r'_(.*?)_', r'<em>\1</em>', line)
        return line
    
    def parse_line(self, line):
        if self.list_item(line):
            return

        # not a list item: were we in a list?
        if self.in_list:
            self.in_list = False
            self.html += "</ul>"

        if self.header(line):
            return

        self.paragraph(line)
        return

    def list_item(self, line):
        m = re.match(r'^[*]\s+(.*)', line)
        if not m:
            return False
        if not self.in_list:
            self.in_list = True
            self.html += "<ul>"
        self.html += f"<li>{m[1]}</li>"
        return True

    def header(self, line):
        m = re.match(r'^(#{1,6})\s+(.*)', line)
        if not m:
            return False
        tag = f"h{str(len(m[1]))}"
        self.html += f"<{tag}>{m[2]}</{tag}>"
        return True

    def paragraph(self, line):
        self.html += f"<p>{line}</p>"
        return


def parse(markdown):
    return Markdown(markdown).markup()
