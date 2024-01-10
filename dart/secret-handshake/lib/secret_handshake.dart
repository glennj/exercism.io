extension on int {
  int bitAt(int offset) => (this >> offset) & 1;
  bool isBitSetAt(int offset) => bitAt(offset) == 1;
}

const actions = <String>['wink', 'double blink', 'close your eyes', 'jump'];

class SecretHandshake {
  List<String> commands(int code) {
    var cmds = <String>[];

    for (var i = 0; i < actions.length; i++) {
      if (code.isBitSetAt(i)) {
        cmds.add(actions[i]);
      }
    }

    if (code.isBitSetAt(actions.length)) {
      cmds = cmds.reversed.toList();
    }

    return cmds;
  }
}
