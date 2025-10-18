import "./num-extension" for NumExt

class LineUp {
  static format(name, number) {
    return "%(name), you are the %(NumExt.nth(number)) customer we serve today. Thank you!"
  }
}
