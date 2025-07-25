import XCTest
import SwiftTreeSitter
import TreeSitterOutrun

final class TreeSitterOutrunTests: XCTestCase {
    func testCanLoadGrammar() throws {
        let parser = Parser()
        let language = Language(language: tree_sitter_outrun())
        XCTAssertNoThrow(try parser.setLanguage(language),
                         "Error loading Outrun grammar")
    }
}
