extension on int {
  int bitAt(int offset) => (this >> offset) & 1;
  bool isBitSetAt(int offset) => bitAt(offset) == 1;
}

const ACTIONS = <String>['wink', 'double blink', 'close your eyes', 'jump'];

class SecretHandshake {
  List<String> commands(int code) {
    var cmds = <String>[];

    for (var i = 0; i < ACTIONS.length; i++)
      if (code.isBitSetAt(i))
        cmds.add(ACTIONS[i]);

    if (code.isBitSetAt(ACTIONS.length))
      cmds = cmds.reversed.toList();

    return cmds;
  }
}
