package main

import "fmt"
import "flag"
import "os"
import "sync"

func main() {
    memory := flag.Int("memory", 1024, "Limitation of memory consumption for goroutine (in bytes)")
    goroutines := flag.Int("goroutines", 10, "Amount of goroutines")

    flag.Parse()
    args := flag.Args()
    if(len(args) == 0){
        fmt.Println("Erro: No file specified")
        os.Exit(1)
    }
    if(len(args) > 1){
        fmt.Println("Erro: Only one file can be specified")
        os.Exit(2)
    }
    file := args[0]

    fd, err := os.Open(file)
    if err != nil {
        fmt.Println("Erro: Could not open file", file)
        os.Exit(3)
    }

    fmt.Println("File:", file)
    fmt.Println("Memory:", *memory)
    fmt.Println("Goroutines:", *goroutines)

    scanner := bufio.NewScanner(fd)
    workQueue := make(chan string)
    wait_group := new(sync.WaitGroup)
}