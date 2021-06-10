# pascal-basic-interpreter

This project is unfinished, for now it can be used as a calculator with an extra feature: contants (they were suppposed to be variables, that's why the keyword is 'var').

## Build
`fpc -O2 MyLanguage.pas`

## Usage
```
$ ./MyLanguage
basic> var i = 6
6
basic> 2 + i * 9
56
basic> 4 - (-2)
6
basic> var f = 1.3
1.3
basic> f + 1.5
2.8
basic> exit
```
