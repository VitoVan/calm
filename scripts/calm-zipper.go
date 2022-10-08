/*
   This file is licensed under the MIT License,
   albeit it is included in the project CALM.

   Some code of this file is copied from `gozip`:
   https://github.com/sanderhahn/gozip

   To build this file for Windows on Unix:

   $ GOOS=windows go build -ldflags -H=windowsgui calm-zipper.go

   To use the compiled binary:

   $ calm-zipper.exe my-awesome-calm-app/dist

   MIT License

   Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

*/

package main

import (
	"archive/zip"
	"crypto/sha256"
	"encoding/hex"
	"errors"
	"fmt"
	"io"
	"io/ioutil"
	"log"
	"os"
	"os/exec"
	"path"
	"path/filepath"
	"strings"
)

func getHash(path string) string {
	f, err := os.Open(path)
	if err != nil {
		log.Fatal(err)
	}
	defer f.Close()

	hasher := sha256.New()
	if _, err := io.Copy(hasher, f); err != nil {
		log.Fatal(err)
	}
	value := hex.EncodeToString(hasher.Sum(nil))
	return value
}

// IsZip checks to see if path is already a zip file
func IsZip(path string) bool {
	r, err := zip.OpenReader(path)
	if err == nil {
		r.Close()
		return true
	}
	return false
}

// Zip takes all the files (dirs) and zips them into path
func Zip(path string, dirs []string) (err error) {
	if IsZip(path) {
		return errors.New(path + " is already a zip file")
	}

	f, err := os.OpenFile(path, os.O_RDWR|os.O_CREATE|os.O_APPEND, 0644)
	if err != nil {
		return
	}
	defer f.Close()

	startoffset, err := f.Seek(0, os.SEEK_END)
	if err != nil {
		return
	}

	w := zip.NewWriter(f)
	w.SetOffset(startoffset)

	for _, dir := range dirs {
		err = filepath.Walk(dir, func(path string, info os.FileInfo, err error) error {
			if err != nil {
				return err
			}

			fh, err := zip.FileInfoHeader(info)
			if err != nil {
				return err
			}
			fh.Name = path

			p, err := w.CreateHeader(fh)
			if err != nil {
				return err
			}
			if !info.IsDir() {
				content, err := ioutil.ReadFile(path)
				if err != nil {
					return err
				}
				_, err = p.Write(content)
				if err != nil {
					return err
				}
			}
			return err
		})
	}
	err = w.Close()
	return
}

// UnzipList Lists all the files in zip file
func UnzipList(path string) (list []string, err error) {
	r, err := zip.OpenReader(path)
	if err != nil {
		return
	}
	for _, f := range r.File {
		list = append(list, f.Name)
	}
	return
}

// Unzip unzips the file zippath and puts it in destination
func Unzip(zippath string, destination string) (err error) {
	r, err := zip.OpenReader(zippath)
	if err != nil {
		return err
	}
	for _, f := range r.File {
		fullname := path.Join(destination, f.Name)
		if f.FileInfo().IsDir() {
			os.MkdirAll(fullname, f.FileInfo().Mode().Perm())
		} else {
			os.MkdirAll(filepath.Dir(fullname), 0755)
			perms := f.FileInfo().Mode().Perm()
			out, err := os.OpenFile(fullname, os.O_CREATE|os.O_RDWR, perms)
			if err != nil {
				return err
			}
			rc, err := f.Open()
			if err != nil {
				return err
			}
			_, err = io.CopyN(out, rc, f.FileInfo().Size())
			if err != nil {
				return err
			}
			rc.Close()
			out.Close()

			mtime := f.FileInfo().ModTime()
			err = os.Chtimes(fullname, mtime, mtime)
			if err != nil {
				return err
			}
		}
	}
	return
}

func main() {
	args := os.Args
	argc := len(args)
	ex, err := os.Executable()
	if err != nil {
		log.Fatal(err)
	}

	fmt.Printf("Executable path: %s\n", ex)

	fmt.Printf("ARGS: %s\n", args)

	fmt.Printf("ARGC: %d\n", argc)

	// no args, unzip and execute calm.exe
	if argc == 1 {
		fmt.Printf("Executing ...\n")

		list, err := UnzipList(ex)
		if err != nil {
			log.Fatal(err)
		}

		calm := "calm.exe"
		for i := range list {
			if strings.Contains(list[i], "calm.exe") {
				calm = list[i]
				break
			}
		}

		fmt.Printf("calm.exe path: %s\n", calm)

		cachedir, direrr := os.UserCacheDir()

		if direrr != nil {
			log.Fatal(direrr)
		}

		dest := cachedir + "/calm-app/" + getHash(ex)

		if _, err := os.Stat(dest); errors.Is(err, os.ErrNotExist) {
			fmt.Printf("Unzipping to %s\n", dest)
			unziperr := Unzip(ex, dest)
			if unziperr != nil {
				log.Fatal(unziperr)
			}
		}
		cmd := exec.Command(dest + "/" + calm)
		fmt.Printf("executing: %s\n", dest+"/"+calm)
		if errors.Is(cmd.Err, exec.ErrDot) {
			cmd.Err = nil
		}
		if err := cmd.Start(); err != nil {
			log.Fatal(err)
		}

	} else if argc == 2 { // 2 arg, zip the directory and create calm app

		fmt.Printf("Packing: %s\n", args[1])

		// copy orignal file
		bytesRead, cperr := ioutil.ReadFile(ex)
		if cperr != nil {
			log.Fatal(cperr)
		}
		cperr = ioutil.WriteFile("calm-app.exe", bytesRead, 0644)
		if cperr != nil {
			log.Fatal(cperr)
		}

		files := []string{}
		err := filepath.Walk(args[1],
			func(path string, info os.FileInfo, err error) error {
				if err != nil {
					return err
				}
				if !info.IsDir() {
					files = append(files, path)
				}
				fmt.Println(path, info.Size())
				return nil
			})
		if err != nil {
			log.Println(err)
		}
		ziperr := Zip("calm-app.exe", files)
		if ziperr != nil {
			log.Fatal(ziperr)
		}
	} else {
		os.Exit(42)
	}
}
