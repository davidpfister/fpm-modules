<a id="readme-top"></a>

[![Contributors][contributors-shield]][contributors-url]
[![Forks][forks-shield]][forks-url]
[![Stargazers][stars-shield]][stars-url]
[![Issues][issues-shield]][issues-url]
[![MIT License][license-shield]][license-url]

<!-- PROJECT LOGO -->
<br />
<div align="center">
  <h3 align="center">fpm-modules</h3>

  <p align="center">
    A fpm plugin to generate module dependency graph
    <br />
    <a href="https://github.com/davidpfister/fpm-modules"><strong>Explore the project »</strong></a>
    <br />
  </p>
</div>

# Introduction
<!-- ABOUT THE PROJECT -->
## About the Project
<center>
<p align="center">
  <img src="https://github.com/davidpfister/fpm-modules/blob/master/.dox/images/force.gif?raw=true" width="512" height="512">
</p>
</center>

**fpm-modules** is a simple plugin for modern fortran to generate module dependency graphs.
<br><br>
Whether you inherited a large piece of code, want to document your code base or have plans to refactor some libraries, being able to visually navigate through the files, modules and dependencies is a must. 
The present project has been created for doing just that. It has been designed to generate a dependency graph and visualize the dependencies between modules. 

* [![fpm][fpm]][fpm-url]
* [![ifort][ifort]][ifort-url]
* [![gfortran][gfortran]][gfortran-url]

Check out the [tutorial](./.dox/tutorial.md) to know more about it!

<!-- GETTING STARTED -->
## Getting Started

### Requirements

To build that library you need

- a Fortran compiler. The following compilers are tested on the default branch of _fpm-modules_:

<center>

| Name |	Version	| Platform	| Architecture |
|:--:|:--:|:--:|:--:|
| GCC Fortran (MinGW) | 14 | Windows 10 | x86_64 |
| Intel oneAPI classic	| 2021.5	| Windows 10 |	x86_64 |

</center>

<!-- USAGE EXAMPLES -->
## Usage

**fpm-modules** has few command lines that can be used: 

>- -d, --dir: The path to the directory where the fpm.toml file seats. </br>
> ```fpm modules -d "./"```
>- -K, --layout: The layout library. Possible options are "dot" (default), "fdp", "sfdp", "neato", "json", "toml", "mermaid" and "force".</br>
> ```fpm modules -K "mermaid"``` 
>- -x, --exclude: The list of excluded packages by name.  Names are comma-separated, no spaces, no quotes.</br>
> ```fpm modules -x fpm,daglib```
>- -o, --output: The path to the output file. By default, the standard output is used.

Here is a table summarizing the different options and combinations that can be used:


|-K|-o|
|:---|:---|
|(default: dot)|*.json, *.dot, *.gv, *.svg, *.jpg, *.png, *.html|
|dot, fdp, sfdp, neato|*.json, *.dot, *.gv, *.svg, *.jpg, *.png, *.html|
|circle|*.json, *.html|
|force|*.json, *.html|
|mermaid|*.mmd, *.html|
|json|*.json|
|toml|*.toml|
|markmap|*.html, *.md|

### Installation

#### Get the code
```bash
git clone https://github.com/davidpfister/fpm-modules
cd fpm-modules
```

#### Build with fpm

The repo can be build using _fpm_
```bash
fpm build
```

#### Installation as fpm plugin

Since it is built with fpm it can easily be installed on your system with
```bash
git clone https://github.com/davidpfister/fpm-modules
cd fpm-modules
fpm install --profile release
```
This will install the fpm-modules binary to ~/.local/bin (or %APPDATA%\local\bin on Windows).

For more information about fpm plugins, visit the [fpm website](https://fpm.fortran-lang.org/tutorial/plugins.html).

<!-- CONTRIBUTING -->
### Contributing

Contributions are what make the open source community such an amazing place to learn, inspire, and create. Any contributions you make are **greatly appreciated**. So, thank you for considering contributing to _fpm-modules_.
Please review and follow these guidelines to make the contribution process simple and effective for all involved. In return, the developers will help address your problem, evaluate changes, and guide you through your pull requests.

By contributing to _fpm-modules_, you certify that you own or are allowed to share the content of your contribution under the same license.

### Style

Please follow the style used in this repository for any Fortran code that you contribute. This allows focusing on substance rather than style.

### Reporting a bug

A bug is a *demonstrable problem* caused by the code in this repository.
Good bug reports are extremely valuable to us—thank you!

Before opening a bug report:

1. Check if the issue has already been reported
   ([issues](https://github.com/davidpfister/fpm-modules/issues)).
2. Check if it is still an issue or it has been fixed?
   Try to reproduce it with the latest version from the default branch.
3. Isolate the problem and create a minimal test case.

A good bug report should include all information needed to reproduce the bug.
Please be as detailed as possible:

1. Which version of _fpm-modules_ are you using? Please be specific.
2. What are the steps to reproduce the issue?
3. What is the expected outcome?
4. What happens instead?

This information will help the developers diagnose the issue quickly and with
minimal back-and-forth.

### Pull request

If you have a suggestion that would make this project better, please create a pull request. You can also simply open an issue with the tag "enhancement".
Don't forget to give the project a star! Thanks again!
1. Open a [new issue](https://github.com/davidpfister/fpm-modules/issues/new) to
   describe a bug or propose a new feature.
   Refer to the earlier sections on how to write a good bug report or feature    request.
2. Discuss with the developers and reach consensus about what should be done about the bug or feature request.
   **When actively working on code towards a PR, please assign yourself to the
   issue on GitHub.**
   This is good collaborative practice to avoid duplicated effort and also inform others what you are currently working on.
3. Create your Feature Branch (```git checkout -b feature/AmazingFeature```)
4. Commit your Changes (```git commit -m 'Add some AmazingFeature'```)
5. Push to the Branch (```git push origin feature/AmazingFeature```)
6. Open a Pull Request with your contribution.
   The body of the PR should at least include a bullet-point summary of the
   changes, and a detailed description is encouraged.
   If the PR completely addresses the issue you opened in step 1, include in
   the PR description the following line: ```Fixes #<issue-number>```. If your PR implements a feature that adds or changes the behavior of _fpm-modules_,
   your PR must also include appropriate changes to the documentation and associated units tests.

In brief, 
* A PR should implement *only one* feature or bug fix.
* Do not commit changes to files that are irrelevant to your feature or bug fix.
* Smaller PRs are better than large PRs, and will lead to a shorter review and
  merge cycle
* Add tests for your feature or bug fix to be sure that it stays functional and useful
* Be open to constructive criticism and requests for improving your code.


<!-- LICENSE -->
## License

Distributed under the MIT License.

<!-- MARKDOWN LINKS & IMAGES -->
[contributors-shield]: https://img.shields.io/github/contributors/davidpfister/fpm-modules.svg?style=for-the-badge
[contributors-url]: https://github.com/davidpfister/fpm-modules/graphs/contributors
[forks-shield]: https://img.shields.io/github/forks/davidpfister/fpm-modules.svg?style=for-the-badge
[forks-url]: https://github.com/davidpfister/fpm-modules/network/members
[stars-shield]: https://img.shields.io/github/stars/davidpfister/fpm-modules.svg?style=for-the-badge
[stars-url]: https://github.com/davidpfister/fpm-modules/stargazers
[issues-shield]: https://img.shields.io/github/issues/davidpfister/fpm-modules.svg?style=for-the-badge
[issues-url]: https://github.com/davidpfister/fpm-modules/issues
[license-shield]: https://img.shields.io/github/license/davidpfister/fpm-modules.svg?style=for-the-badge
[license-url]: https://github.com/davidpfister/fpm-modules/master/LICENSE
[gfortran]: https://img.shields.io/badge/gfortran-000000?style=for-the-badge&logo=gnu&logoColor=white
[gfortran-url]: https://gcc.gnu.org/wiki/GFortran
[ifort]: https://img.shields.io/badge/ifort-000000?style=for-the-badge&logo=Intel&logoColor=61DAFB
[ifort-url]: https://www.intel.com/content/www/us/en/developer/tools/oneapi/fortran-compiler.html
[fpm]: https://img.shields.io/badge/fpm-000000?style=for-the-badge&logo=Fortran&logoColor=734F96
[fpm-url]: https://fpm.fortran-lang.org/
