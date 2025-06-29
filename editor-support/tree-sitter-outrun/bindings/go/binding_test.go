package tree_sitter_outrun_test

import (
	"testing"

	tree_sitter "github.com/tree-sitter/go-tree-sitter"
	tree_sitter_outrun "harton.dev/outrun/outrun/bindings/go"
)

func TestCanLoadGrammar(t *testing.T) {
	language := tree_sitter.NewLanguage(tree_sitter_outrun.Language())
	if language == nil {
		t.Errorf("Error loading Outrun grammar")
	}
}
