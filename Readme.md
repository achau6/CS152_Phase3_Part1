CS152 Project Phase 3 - Code Generation
========================================

[Home Page - Phase3 Code Generation Part1](https://www.cs.ucr.edu/~dtan004/proj3/phase3_code_generator.html)

[Home Page - Phase3 Code Generation Part2](https://www.cs.ucr.edu/~dtan004/proj4/phase3_code_generator.html)


## Tools preparation

Make sure you have a Linux environment for this project. You can use 'bolt', your own Linux machine, or Windows Subsystem for Linux(WSL). We highly recommend you directly use 'bolt' since it contains all the necessary tools preinstalled. 

```sh
ssh <your-net-id>@bolt.cs.ucr.edu
```

Make sure you have the following tools installed and check the version:
1. flex -V       (>=2.5)
2. bison -V      (>=3.0)
3. git --version (>=1.8)
4. make -v       (>=3.8)
5. gcc -v        (>=4.8)
6. g++ -v        (>=4.8)

## Clone and Build

Use 'git' to clone the project template and copy your code in phase 2 into this new repository.

```sh
    git clone <your-repo-link> phase3
    cd phase3 && make
```

## Use the template

You can change any files and add additional C/C++ files, but please make sure all files are linked to the final executable file in Makefile. Please don't change the name of 'Makefile' , 'miniL.lex' and 'miniL.y'. After typing make, An executable file 'miniL' is created as your miniL language compiler. Please don't change the output target name 'miniL' also.

You can add additional C++ class definition in the file `lib.h` and implementations in `lib.cpp`.

It's possible to add more your custom `*.cpp` files by editing the Makefile. List them in the `SRC` variable in Makefile. 

`min_c` is the correct implementation of miniL compiler. You can use it to generate more test cases if you need.

`min_run` is the same MIL interpreter in our home page.

## Project Submission 

* Start Date:           2/07/21
* Due Date for part1:   2/20/21
* Due Date for part2:   3/06/21

Project submission requires you to commit and push your code to Github after finishing the phase3 project. As the following steps:
```sh
git status # it should be under 'main' branch (otherwise you should switch to 'main' branch by typing 'git checkout main')
git add .  # add all files under current folder into staged changes
git commit -m "phase1 submission"  # make a commit for your changes
git push   # push all local commits to Github, this step may need your username and password of Github
```

For phase3-part1 submission, please create a new Release, then you can give it a name Part1 to indicate this version is your final version of part1. After creating an release, you can continue pushing new commits and do not need to worry your new commits will influence your part1 grading. For part2 submission, you can just keep your final version of code in your main branch.

![image](https://user-images.githubusercontent.com/6300305/152875849-ed6f8491-bcb9-468e-8fca-626732aa06e6.png)
![image](https://user-images.githubusercontent.com/6300305/152875944-c2c4df84-e445-4ce3-be75-3e6a342ff8ef.png)


Before the deadline, you can create commits and push to Github without any limitation. Please make sure your code under 'main' branch is the last version you wish to submit. After the deadline, please do not push new commits to 'main' branch otherwise it will be considered as a late submission.

![image](https://user-images.githubusercontent.com/6300305/148487911-7bcaa3ac-2c8e-4ee3-9ead-ab908d5e1710.png)


## Project Demo and Grading

The project demo will be held in the next lab section after the due date. All teams should attend it since it's important to ensure that your code can move smoothly to the next phase. Contact your TA if you need to change your demo time. 
You grade will be updated on eLearn (usually in 1 week after your demo time). 

| Grading rules | Points(%) |
| ---------------- | -- |
|Successfully Build| 10%|
|Correctness       | 80%|
|Demo              | 10%|

## Academic integrity

Copying source code from students in previous semester and other teams will be considered a violation of academic integrity. 
