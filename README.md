# Monolith

## Language reference

### Exports

```
export
    add
    subtract
    reverse
    map
    filter
```

### Imports

```
import
    Math
        add
        subtract
    String
        capitalize
    HashMap
        insert
        get
```

### Top level declarations

```
decimal = 3.1415926535
natural = 420
base16  = 0x1A4
base8   = 0o644
base2   = 0b110100100
```

### Math operators, precedence and associativity

```
expression = 4 + 5 - 9.4 * 2 / 5 ^ 3
```

| Symbol | Operation      | Precedence | Associativity |
| ------ | -------------- | ---------- | ------------- |
| `+`    | Addition       | 7          | Left          |
| `-`    | Subtraction    | 7          | Left          |
| `*`    | Multiplication | 8          | Left          |
| `/`    | Division       | 8          | Left          |
| `^`    | Exponentiation | 9          | Right         |


