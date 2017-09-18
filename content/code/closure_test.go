package bench

import (
	"fmt"
	"regexp"
	"strings"
	"testing"
)

var vowels = map[rune]bool{
	'a': true, 'e': true, 'i': true,
	'o': true, 'u': true, 'y': true,
}
var rxGolang = regexp.MustCompile(`[Gg]o|[Gg]golang`)

func hasVowel(s string) bool {
	for _, c := range s {
		if vowels[c] {
			return true
		}
	}
	return false
}
func describeString1(s string) string {
	var attrs []string
	if hasVowel(s) {
		attrs = append(attrs, "has vowel letter")
	}
	if rxGolang.MatchString(s) {
		attrs = append(attrs, "may be about Go language")
	}
	attrs = append(attrs, fmt.Sprintf("has length of %d", len(s)))
	return strings.Join(attrs, "; ")
}

var describeString2 = func() func(name string) string {
	vowels := map[rune]bool{
		'a': true, 'e': true, 'i': true,
		'o': true, 'u': true, 'y': true,
	}
	rxGolang := regexp.MustCompile(`[Gg]o|[Gg]golang`)
	hasVowel := func(s string) bool {
		for _, c := range s {
			if vowels[c] {
				return true
			}
		}
		return false
	}

	return func(s string) string {
		var attrs []string
		if hasVowel(s) {
			attrs = append(attrs, "has vowel letter")
		}
		if rxGolang.MatchString(s) {
			attrs = append(attrs, "may be about Go language")
		}
		attrs = append(attrs, fmt.Sprintf("has length of %d", len(s)))
		return strings.Join(attrs, "; ")
	}
}()

var discardResult string

func BenchmarkNormalFunc(b *testing.B) {
	for i := 0; i < b.N; i++ {
		for _, s := range input {
			discardResult = describeString1(s)
		}
	}
}

func BenchmarkClosure(b *testing.B) {
	for i := 0; i < b.N; i++ {
		for _, s := range input {
			discardResult = describeString2(s)
		}
	}
}

var input = []string{
	"4th Dimension/4D",
	"ABAP",
	"ABC",
	"ActionScript",
	"Ada",
	"Agilent VEE",
	"Algol",
	"Alice",
	"Angelscript",
	"Apex",
	"APL",
	"AppleScript",
	"Arc",
	"Arduino",
	"ASP",
	"AspectJ",
	"Assembly",
	"ATLAS",
	"Augeas",
	"AutoHotkey",
	"AutoIt",
	"AutoLISP",
	"Automator",
	"Avenue",
	"Awk",
	"Bash",
	"(Visual) Basic",
	"bc",
	"BCPL",
	"BETA",
	"BlitzMax",
	"Boo",
	"Bourne Shell",
	"Bro",
	"C",
	"C Shell",
	"C#",
	"C++",
	"C++/CLI",
	"C-Omega",
	"Caml",
	"Ceylon",
	"CFML",
	"cg",
	"Ch",
	"CHILL",
	"CIL",
	"CL (OS/400)",
	"Clarion",
	"Clean",
	"Clipper",
	"Clojure",
	"CLU",
	"COBOL",
	"Cobra",
	"CoffeeScript",
	"ColdFusion",
	"COMAL",
	"Common Lisp",
	"Coq",
	"cT",
	"Curl",
	"D",
	"Dart",
	"DCL",
	"DCPU-16 ASM",
	"Delphi/Object Pascal",
	"DiBOL",
	"Dylan",
	"E",
	"eC",
	"Ecl",
	"ECMAScript",
	"EGL",
	"Eiffel",
	"Elixir",
	"Emacs Lisp",
	"Erlang",
	"Etoys",
	"Euphoria",
	"EXEC",
	"F#",
	"Factor",
	"Falcon",
	"Fancy",
	"Fantom",
	"Felix",
	"Forth",
	"Fortran",
	"Fortress",
	"(Visual) FoxPro",
	"Gambas",
	"GNU Octave",
	"Go",
	"Google AppsScript",
	"Gosu",
	"Groovy",
	"Haskell",
	"haXe",
	"Heron",
}

