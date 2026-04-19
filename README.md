# Gale-Shapley / McVitie-Wilson Algorithm in Scheme

Solving the stable matching problem for residents and medical programs. McVitie-Wilson is used since Scheme is highly suitable for recursion. `main.scm` contains the main logic and `rpReader.scm` helps read CSV files (not included in this repo).

## Gale-Shapley
* Breadth-first
* Unassigned residents iterate through their preference lists one by one, making `offer` to top available program
* Programs `evaluate` these offers based on ranking requirements and quota capacities

## McVitie-Wilson
* Depth-first
* When a applicant applies to a program through `evaluate`, the program accepts the resident if there is capacity
* If quota is full, it compares the applicant's rank against the current worst ranked and accepted resident. If the applicant is better, the program removes the worst resident and accepts that as the new resident
* Algorithm immediately triggers a recursive `offer` call for the displaced resident

## How to Run
Using Racket installed on your machine:
```bash
racket
```
```repl
(require "main.scm")
(your_function)
```

If you wish to write the output to a text file:
```repl
(with-output-to-file "output.txt" (lambda () (display(time(gale-shapley RLIST PLIST '() #:exists 'replace)))))
```