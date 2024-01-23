# | Register | Usage  | Type    | Description            |
# | -------- | ------ | ------- | ---------------------- |
# | `$v0`    | output | address | null-terminated string |

.globl hello

.data

msg: .asciiz "Hello, World!"

.text

hello:
        la $v0, msg
        jr $ra
