#include "queen_attack.h"
#include <stdlib.h>

attack_status_t can_attack(position_t q1, position_t q2) {
    // not on the board
    if (q1.column >= BOARD_SIZE || q1.row >= BOARD_SIZE ||
        q2.column >= BOARD_SIZE || q2.row >= BOARD_SIZE
    ) return INVALID_POSITION;

    // same position
    if (q1.column == q2.column && q1.row == q2.row)
        return INVALID_POSITION;

    // can attack
    if ( q1.column == q2.column ||
         q1.row    == q2.row    ||
         abs(q1.column - q2.column) == abs(q1.row - q2.row)
    ) return CAN_ATTACK;

    return CAN_NOT_ATTACK;
}
