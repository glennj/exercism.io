"use strict";
exports.__esModule = true;
var RobotName = /** @class */ (function () {
    function RobotName() {
    }
    RobotName.prototype.contructor = function () {
        this._name = this.generateName();
        console.log("constructor: " + this.name);
    };
    Object.defineProperty(RobotName.prototype, "name", {
        get: function () { return this._name; },
        enumerable: true,
        configurable: true
    });
    RobotName.prototype.generateName = function () {
        var name;
        do {
            name = this.randA() + this.randA()
                + this.randD() + this.randD() + this.randD();
        } while (RobotName.names.has(name));
        console.log(name);
        RobotName.names.add(name);
        console.log(RobotName.names.values().slice());
        return name;
    };
    RobotName.prototype.randA = function () {
        return String.fromCharCode(64 + Math.ceil(Math.random() * 26));
    };
    RobotName.prototype.randD = function () {
        return String(Math.floor(Math.random() * 10));
    };
    RobotName.prototype.resetName = function () {
        var newName = this.generateName();
        RobotName.names["delete"](this.name);
        RobotName.names.add(newName);
        this._name = newName;
    };
    RobotName.names = new Set();
    return RobotName;
}());
exports["default"] = RobotName;
