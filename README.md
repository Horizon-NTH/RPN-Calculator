# RPN-Calculator

[![Release](https://img.shields.io/badge/Release-v1.0-blueviolet)](https://github.com/Horizon-NTH/RPN-Calculator/releases)
[![Language](https://img.shields.io/badge/Language-Assembly-0052cf)](https://en.wikipedia.org/wiki/Assembly_language)
[![Licence](https://img.shields.io/badge/License-MIT-yellow)](LICENSE)

![RPN calculator Logo](./assets/rpn_calculator.png)

## Introduction

An **RPN calculator** programmed in [ARM7TDMI](https://en.wikipedia.org/wiki/ARM7#ARM7TDMI) Assembly. It supports numerous operators and also incorporates 
[floating-point arithmetic](https://en.wikipedia.org/wiki/Floating-point_arithmetic), utilizing the [IEEE 754](https://en.wikipedia.org/wiki/IEEE_754) standard.

## Installation Instructions

> You can also simply install the release version [here](https://github.com/Horizon-NTH/RPN-Calculator/releases).

### Get Source Code

You first need to clone the repository. Make sure to use [git](https://git-scm.com).

```bash
git clone https://github.com/Horizon-NTH/RPN-Calculator.git
```

### Get the simulator

Although this program has been written in ARM7TDMI language, it incorporates specific features tailored to operate 
within this [simulator](https://github.com/mgard/epater). Therefore, please install the simulator by following the 
instructions provided on its GitHub page and refer to its documentation to explore all the specifications.

>⚠️ To make the code work on the simulator, you need to modify the address of the data section in the [`assembler.py`](https://github.com/mgard/epater/blob/master/assembler.py) 
> file and set it to a minimum of `0x1500`. 
> You need to modify the value of `DATA` on line 12 as shown in the following code snippet.

```python
memory_configs = {
    "simulation": {"INTVEC": 0x00, "CODE": 0x80, "DATA": 0x1500},
    "test": {"INTVEC": 0x100000, "CODE": 0x100080, "DATA": 0x101000}
}
```

>Additionally, to run the program entirely, you will need to either press the execution button multiple times or 
> increase the value of `runmaxit` in the [`settings.py`](https://github.com/mgard/epater/blob/master/settings.py) file, at line 10.

```python
_settings = {"PCbehavior": "+8",                
             "allowuserswitchmode": True,                
             "runmaxit": 10000,
             "maxhistorylength": 1000,
             "fillValue": 0xFF,
             "maxtotalmem": 0x10000,
             }
```

## Documentation

To see how to use the calculator or how does it work, please refer to the [wiki](https://github.com/Horizon-NTH/RPN-Calculator/wiki).

## Dependencies

- **[epater](https://github.com/mgard/epater)** is the simulator used to run the code.

## License

This project is licensed under the [MIT license](LICENSE).