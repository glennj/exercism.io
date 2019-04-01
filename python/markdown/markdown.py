import re


def parse_markdown(markdown):
    html = ''
    in_list = False

    # function is nested for access to nonlocal variables
    def list_item(line):
        nonlocal html, in_list, is_paragraph
        m = re.match(r'^\*\s+(.*)', line)
        if m:
            line = f"<li>{m[1]}</li>"
            if not in_list:
                line = f"<ul>{line}"
            in_list = True
            is_paragraph = False
        else:
            if in_list:
                html += "</ul>"
                in_list = False
        return (line, is_paragraph)

    # now, parse
    for line in markdown.split('\n'):
        is_paragraph = True     #
        html += paragraph(*heading(*list_item(style(line))))

    if in_list:
        html += "</ul>"
    return html


def style(line):
    line = re.sub(r'__(.*?)__', r'<strong>\1</strong>', line)
    line = re.sub(r'_(.*?)_', r'<em>\1</em>', line)
    return line


def heading(line, is_paragraph):
    def heading_repl(m):
        nonlocal is_paragraph
        is_paragraph = False
        tag = "h" + str(len(m[1]))
        return f"<{tag}>{m[2]}</{tag}>"

    line = re.sub(r'^(#{1,6})\s+(.*)', heading_repl, line)
    return (line, is_paragraph)


def paragraph(line, is_paragraph):
    if is_paragraph:
        line = f"<p>{line}</p>"
    return line
