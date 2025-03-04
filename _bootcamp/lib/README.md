# Exercism Bootcamp Custom Functions (stdlib)

Dependency relationships:

```mermaid
erDiagram
    NULL ||--|| INDEX_OF : ""
    NULL ||--|| CHARS : ""
    NULL ||--|| IS_DIVISIBLE_BY : ""
    NULL ||--|| IS_EMPTY : ""
    NULL ||--|| IS_EVEN : ""
    NULL ||--|| HAS_KEY : ""
    NULL ||--|| LENGTH : ""
    NULL ||--|| SPLIT : ""
    NULL ||--|| STR_DROP : ""
    NULL ||--|| STR_TAKE : ""
    INDEX_OF ||--|| CONTAINS : ""
    CONTAINS ||--|| IS_LOWER : ""
    CONTAINS ||--|| IS_UPPER : ""
    CONTAINS ||--|| IS_DIGIT : ""
    IS_LOWER ||--|| IS_ALPHA : ""
    IS_UPPER ||--|| IS_ALPHA : ""
    IS_ALPHA ||--|| IS_ALNUM : ""
    IS_DIGIT ||--|| IS_ALNUM : ""
    IS_LOWER ||--|| TO_UPPER : ""
    IS_UPPER ||--|| TO_LOWER : ""
    HAS_KEY ||--|| DICT_INCR : ""
    LENGTH ||--|| ENDS_WITH : ""
    LENGTH ||--|| JOIN : ""
    LENGTH ||--|| LREVERSE : ""
    LENGTH ||--|| REMOVE_AT : ""
    LENGTH ||--|| STARTS_WITH : ""
    LENGTH ||--|| TO_SENTENCE : ""
    SPLIT ||--|| STR_TOKENIZE : ""
```

![dependency relationships](./dependency.png)
