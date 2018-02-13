module Test.JSON where

import           JSON
import           System.IO
import           Test.Tasty
import           Test.Tasty.HUnit

test_parseJson :: IO [TestTree]
test_parseJson = sequence $ fmap check testSuite

check :: (FilePath, Maybe JSON) -> IO TestTree
check (path, ans) = do
  handle <- openFile ("test/test_parsing/" ++ path) ReadMode
  hSetEncoding handle utf8
  str <- hGetContents handle
  pure $ testCase ("check: " ++ path) (fromRight (parseJson str) @?= ans)

fromRight :: Either e a -> Maybe a
fromRight = either (const Nothing) Just

testSuite :: [(FilePath, Maybe JSON)]
testSuite =
  [ ("n_array_1_true_without_comma.json", Nothing)
  -- , ("n_array_a_invalid_utf8.json", Nothing)
  , ("n_array_colon_instead_of_comma.json", Nothing)
  , ("n_array_comma_after_close.json", Nothing)
  , ("n_array_comma_and_number.json", Nothing)
  , ("n_array_double_comma.json", Nothing)
  , ("n_array_double_extra_comma.json", Nothing)
  , ("n_array_extra_close.json", Nothing)
  , ("n_array_extra_comma.json", Nothing)
  , ("n_array_incomplete.json", Nothing)
  , ("n_array_incomplete_invalid_value.json", Nothing)
  , ("n_array_inner_array_no_comma.json", Nothing)
  -- , ("n_array_invalid_utf8.json", Nothing)
  , ("n_array_items_separated_by_semicolon.json", Nothing)
  , ("n_array_just_comma.json", Nothing)
  , ("n_array_just_minus.json", Nothing)
  , ("n_array_missing_value.json", Nothing)
  , ("n_array_newlines_unclosed.json", Nothing)
  , ("n_array_number_and_comma.json", Nothing)
  , ("n_array_number_and_several_commas.json", Nothing)
  , ("n_array_spaces_vertical_tab_formfeed.json", Nothing)
  , ("n_array_star_inside.json", Nothing)
  , ("n_array_unclosed.json", Nothing)
  , ("n_array_unclosed_trailing_comma.json", Nothing)
  , ("n_array_unclosed_with_new_lines.json", Nothing)
  , ("n_array_unclosed_with_object_inside.json", Nothing)
  , ("n_incomplete_false.json", Nothing)
  , ("n_incomplete_null.json", Nothing)
  , ("n_incomplete_true.json", Nothing)
  , ("n_multidigit_number_then_00.json", Nothing)
  , ("n_number_++.json", Nothing)
  , ("n_number_+1.json", Nothing)
  , ("n_number_+Inf.json", Nothing)
  , ("n_number_-01.json", Nothing)
  , ("n_number_-1.0..json", Nothing)
  , ("n_number_-2..json", Nothing)
  , ("n_number_-NaN.json", Nothing)
  , ("n_number_.-1.json", Nothing)
  , ("n_number_.2e-3.json", Nothing)
  , ("n_number_0.1.2.json", Nothing)
  , ("n_number_0.3e+.json", Nothing)
  , ("n_number_0.3e.json", Nothing)
  , ("n_number_0.e1.json", Nothing)
  , ("n_number_0e+.json", Nothing)
  , ("n_number_0e.json", Nothing)
  , ("n_number_0_capital_E+.json", Nothing)
  , ("n_number_0_capital_E.json", Nothing)
  , ("n_number_1.0e+.json", Nothing)
  , ("n_number_1.0e-.json", Nothing)
  , ("n_number_1.0e.json", Nothing)
  , ("n_number_1eE2.json", Nothing)
  , ("n_number_1_000.json", Nothing)
  , ("n_number_2.e+3.json", Nothing)
  , ("n_number_2.e-3.json", Nothing)
  , ("n_number_2.e3.json", Nothing)
  , ("n_number_9.e+.json", Nothing)
  , ("n_number_expression.json", Nothing)
  , ("n_number_hex_1_digit.json", Nothing)
  , ("n_number_hex_2_digits.json", Nothing)
  , ("n_number_Inf.json", Nothing)
  , ("n_number_infinity.json", Nothing)
  , ("n_number_invalid+-.json", Nothing)
  , ("n_number_invalid-negative-real.json", Nothing)
  -- , ("n_number_invalid-utf-8-in-bigger-int.json", Nothing)
  -- , ("n_number_invalid-utf-8-in-exponent.json", Nothing)
  -- , ("n_number_invalid-utf-8-in-int.json", Nothing)
  , ("n_number_minus_infinity.json", Nothing)
  , ("n_number_minus_sign_with_trailing_garbage.json", Nothing)
  , ("n_number_minus_space_1.json", Nothing)
  , ("n_number_NaN.json", Nothing)
  , ("n_number_neg_int_starting_with_zero.json", Nothing)
  , ("n_number_neg_real_without_int_part.json", Nothing)
  , ("n_number_neg_with_garbage_at_end.json", Nothing)
  , ("n_number_real_garbage_after_e.json", Nothing)
  , ("n_number_real_without_fractional_part.json", Nothing)
  -- , ("n_number_real_with_invalid_utf8_after_e.json", Nothing)
  , ("n_number_starting_with_dot.json", Nothing)
  , ("n_number_U+FF11_fullwidth_digit_one.json", Nothing)
  , ("n_number_with_alpha.json", Nothing)
  , ("n_number_with_alpha_char.json", Nothing)
  , ("n_number_with_leading_zero.json", Nothing)
  , ("n_object_bad_value.json", Nothing)
  , ("n_object_bracket_key.json", Nothing)
  , ("n_object_comma_instead_of_colon.json", Nothing)
  , ("n_object_double_colon.json", Nothing)
  , ("n_object_emoji.json", Nothing)
  , ("n_object_garbage_at_end.json", Nothing)
  , ("n_object_key_with_single_quotes.json", Nothing)
  , ("n_object_missing_colon.json", Nothing)
  , ("n_object_missing_key.json", Nothing)
  , ("n_object_missing_semicolon.json", Nothing)
  , ("n_object_missing_value.json", Nothing)
  , ("n_object_no-colon.json", Nothing)
  , ("n_object_non_string_key.json", Nothing)
  , ("n_object_non_string_key_but_huge_number_instead.json", Nothing)
  -- , ("n_object_pi_in_key_and_trailing_comma.json", Nothing)
  , ("n_object_repeated_null_null.json", Nothing)
  , ("n_object_several_trailing_commas.json", Nothing)
  , ("n_object_single_quote.json", Nothing)
  , ("n_object_trailing_comma.json", Nothing)
  , ("n_object_trailing_comment.json", Nothing)
  , ("n_object_trailing_comment_open.json", Nothing)
  , ("n_object_trailing_comment_slash_open.json", Nothing)
  , ("n_object_trailing_comment_slash_open_incomplete.json", Nothing)
  , ("n_object_two_commas_in_a_row.json", Nothing)
  , ("n_object_unquoted_key.json", Nothing)
  , ("n_object_unterminated-value.json", Nothing)
  , ("n_object_with_single_string.json", Nothing)
  , ("n_object_with_trailing_garbage.json", Nothing)
  , ("n_single_space.json", Nothing)
  , ("n_string_1_surrogate_then_escape.json", Nothing)
  , ("n_string_1_surrogate_then_escape_u.json", Nothing)
  , ("n_string_1_surrogate_then_escape_u1.json", Nothing)
  , ("n_string_1_surrogate_then_escape_u1x.json", Nothing)
  , ("n_string_accentuated_char_no_quotes.json", Nothing)
  , ("n_string_backslash_00.json", Nothing)
  , ("n_string_escaped_backslash_bad.json", Nothing)
  , ("n_string_escaped_ctrl_char_tab.json", Nothing)
  , ("n_string_escaped_emoji.json", Nothing)
  , ("n_string_escape_x.json", Nothing)
  , ("n_string_incomplete_escape.json", Nothing)
  , ("n_string_incomplete_escaped_character.json", Nothing)
  , ("n_string_incomplete_surrogate.json", Nothing)
  , ("n_string_incomplete_surrogate_escape_invalid.json", Nothing)
  -- , ("n_string_invalid-utf-8-in-escape.json", Nothing)
  , ("n_string_invalid_backslash_esc.json", Nothing)
  , ("n_string_invalid_unicode_escape.json", Nothing)
  -- , ("n_string_invalid_utf8_after_escape.json", Nothing)
  , ("n_string_leading_uescaped_thinspace.json", Nothing)
  , ("n_string_no_quotes_with_bad_escape.json", Nothing)
  , ("n_string_single_doublequote.json", Nothing)
  , ("n_string_single_quote.json", Nothing)
  , ("n_string_single_string_no_double_quotes.json", Nothing)
  , ("n_string_start_escape_unclosed.json", Nothing)
  , ("n_string_unescaped_crtl_char.json", Nothing)
  , ("n_string_unescaped_newline.json", Nothing)
  , ("n_string_unescaped_tab.json", Nothing)
  , ("n_string_unicode_CapitalU.json", Nothing)
  , ("n_string_with_trailing_garbage.json", Nothing)
  , ("n_structure_100000_opening_arrays.json", Nothing)
  , ("n_structure_angle_bracket_..json", Nothing)
  , ("n_structure_angle_bracket_null.json", Nothing)
  , ("n_structure_array_trailing_garbage.json", Nothing)
  , ("n_structure_array_with_extra_array_close.json", Nothing)
  , ("n_structure_array_with_unclosed_string.json", Nothing)
  , ("n_structure_ascii-unicode-identifier.json", Nothing)
  , ("n_structure_capitalized_True.json", Nothing)
  , ("n_structure_close_unopened_array.json", Nothing)
  , ("n_structure_comma_instead_of_closing_brace.json", Nothing)
  , ("n_structure_double_array.json", Nothing)
  , ("n_structure_end_array.json", Nothing)
  -- , ("n_structure_incomplete_UTF8_BOM.json", Nothing)
  -- , ("n_structure_lone-invalid-utf-8.json", Nothing)
  , ("n_structure_lone-open-bracket.json", Nothing)
  , ("n_structure_no_data.json", Nothing)
  , ("n_structure_null-byte-outside-string.json", Nothing)
  , ("n_structure_number_with_trailing_garbage.json", Nothing)
  , ("n_structure_object_followed_by_closing_object.json", Nothing)
  , ("n_structure_object_unclosed_no_value.json", Nothing)
  , ("n_structure_object_with_comment.json", Nothing)
  , ("n_structure_object_with_trailing_garbage.json", Nothing)
  , ("n_structure_open_array_apostrophe.json", Nothing)
  , ("n_structure_open_array_comma.json", Nothing)
  , ("n_structure_open_array_object.json", Nothing)
  , ("n_structure_open_array_open_object.json", Nothing)
  , ("n_structure_open_array_open_string.json", Nothing)
  , ("n_structure_open_array_string.json", Nothing)
  , ("n_structure_open_object.json", Nothing)
  , ("n_structure_open_object_close_array.json", Nothing)
  , ("n_structure_open_object_comma.json", Nothing)
  , ("n_structure_open_object_open_array.json", Nothing)
  , ("n_structure_open_object_open_string.json", Nothing)
  , ("n_structure_open_object_string_with_apostrophes.json", Nothing)
  , ("n_structure_open_open.json", Nothing)
  -- , ("n_structure_single_eacute.json", Nothing)
  , ("n_structure_single_star.json", Nothing)
  , ("n_structure_trailing_#.json", Nothing)
  , ("n_structure_U+2060_word_joined.json", Nothing)
  , ("n_structure_uescaped_LF_before_string.json", Nothing)
  , ("n_structure_unclosed_array.json", Nothing)
  , ("n_structure_unclosed_array_partial_null.json", Nothing)
  , ("n_structure_unclosed_array_unfinished_false.json", Nothing)
  , ("n_structure_unclosed_array_unfinished_true.json", Nothing)
  , ("n_structure_unclosed_object.json", Nothing)
  , ("n_structure_unicode-identifier.json", Nothing)
  -- , ("n_structure_UTF8_BOM_no_data.json", Nothing)
  -- , ("n_structure_whitespace_formfeed.json", Nothing)
  , ("n_structure_whitespace_U+2060_word_joiner.json", Nothing)
  , ("y_array_arraysWithSpaces.json", Just $ JArray [JArray []])
  , ("y_array_empty-string.json", Just $ JArray [JString ""])
  , ("y_array_empty.json", Just $ JArray [])
  , ("y_array_ending_with_newline.json", Just $ JArray [JString "a"])
  , ("y_array_false.json", Just $ JArray [JBool False])
  , ("y_array_heterogeneous.json", Just $ JArray [JNull, JNumber 1.0, JString "1", JObject []])
  , ("y_array_null.json", Just $ JArray [JNull])
  , ("y_array_with_1_and_newline.json", Just $ JArray [JNumber 1.0])
  , ("y_array_with_leading_space.json", Just $ JArray [JNumber 1.0])
  , ("y_array_with_several_null.json", Just $ JArray [JNumber 1.0, JNull, JNull, JNull, JNumber 2.0])
  , ("y_array_with_trailing_space.json", Just $ JArray [JNumber 2.0])
  , ("y_number.json", Just $ JArray [JNumber 1.23e67])
  , ("y_number_0e+1.json", Just $ JArray [JNumber 0.0])
  , ("y_number_0e1.json",  Just $ JArray [JNumber 0.0])
  , ("y_number_after_space.json", Just $ JArray [JNumber 4.0])
  , ("y_number_double_close_to_zero.json", Just $ JArray [JNumber (-1.0e-78)])
  , ("y_number_int_with_exp.json", Just $ JArray [JNumber 200.0])
  , ("y_number_minus_zero.json", Just $ JArray [JNumber (-0.0)])
  , ("y_number_negative_int.json", Just $ JArray [JNumber (-123.0)])
  , ("y_number_negative_one.json", Just $ JArray [JNumber (-1.0)])
  , ("y_number_negative_zero.json", Just $ JArray [JNumber (-0.0)])
  , ("y_number_real_capital_e.json", Just $ JArray [JNumber 1.0e22])
  , ("y_number_real_capital_e_neg_exp.json", Just $ JArray [JNumber 1.0e-2])
  , ("y_number_real_capital_e_pos_exp.json", Just $ JArray [JNumber 100.0])
  , ("y_number_real_exponent.json", Just $ JArray [JNumber 1.23e47])
  , ("y_number_real_fraction_exponent.json", Just $ JArray [JNumber 1.23456e80])
  , ("y_number_real_neg_exp.json", Just $ JArray [JNumber 1.0e-2])
  , ("y_number_real_pos_exponent.json", Just $ JArray [JNumber 100.0])
  , ("y_number_simple_int.json", Just $ JArray [JNumber 123.0])
  , ("y_number_simple_real.json", Just $ JArray [JNumber 123.456789])
  , ("y_object.json", Just $ JObject [("asd", JString "sdf"), ("dfg", JString "fgh")])
  , ("y_object_basic.json", Just $ JObject [("asd", JString "sdf")])
  , ("y_object_duplicated_key.json", Just $ JObject [("a", JString "b"), ("a", JString "c")])
  , ("y_object_duplicated_key_and_value.json", Just $ JObject [("a", JString "b"), ("a", JString "b")])
  , ("y_object_empty.json", Just $ JObject [])
  , ("y_object_empty_key.json", Just $ JObject [("", JNumber 0.0)])
  , ("y_object_escaped_null_in_key.json", Just $ JObject [("foo\NULbar", JNumber 42.0)])
  , ("y_object_extreme_numbers.json", Just $ JObject [("min", JNumber (-1.0e28)), ("max", JNumber 1.0e28)])
  , ("y_object_long_strings.json", Just $ JObject [("x", JArray [JObject [("id", JString "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx")]]), ("id", JString "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx")])
  , ("y_object_simple.json", Just $ JObject [("a", JArray [])])
  , ("y_object_string_unicode.json", Just $ JObject [("title", JString "Полтора Землекопа")])
  , ("y_object_with_newlines.json", Just $ JObject [("a", JString "b")])
  , ("y_string_1_2_3_bytes_UTF-8_sequences.json", Just $ JArray [JString "`Īካ"])
  , ("y_string_accepted_surrogate_pair.json", Just $ JArray [JString "\xD801\xdc37"])
  , ("y_string_accepted_surrogate_pairs.json", Just $ JArray [JString "\xd83d\xde39\xd83d\xdc8d"])
  , ("y_string_allowed_escapes.json", Just $ JArray [JString "\"\\/\b\f\n\r\t"])
  , ("y_string_backslash_and_u_escaped_zero.json", Just $ JArray [JString "\\u0000"])
  , ("y_string_backslash_doublequotes.json", Just $ JArray [JString "\""])
  , ("y_string_comments.json", Just $ JArray [JString "a/*b*/c/*d//e"])
  , ("y_string_double_escape_a.json", Just $ JArray [JString "\\a"])
  , ("y_string_double_escape_n.json", Just $ JArray [JString "\\n"])
  , ("y_string_escaped_control_character.json", Just $ JArray [JString "\DC2"])
  , ("y_string_escaped_noncharacter.json", Just $ JArray [JString "\xFFFF"])
  , ("y_string_in_array.json", Just $ JArray [JString "asd"])
  , ("y_string_in_array_with_leading_space.json", Just $ JArray [JString "asd"])
  , ("y_string_last_surrogates_1_and_2.json", Just $ JArray [JString "\xDBFF\xDFFF"])
  , ("y_string_nbsp_uescaped.json", Just $ JArray [JString "new\x00A0line"])
  , ("y_string_nonCharacterInUTF-8_U+10FFFF.json", Just $ JArray [JString "\x10FFFF"])
  , ("y_string_nonCharacterInUTF-8_U+1FFFF.json", Just $ JArray [JString "\x1BFFF"])
  , ("y_string_nonCharacterInUTF-8_U+FFFF.json", Just $ JArray [JString "\xFFFF"])
  , ("y_string_null_escape.json", Just $ JArray [JString "\NUL"])
  , ("y_string_one-byte-utf-8.json", Just $ JArray [JString ","])
  , ("y_string_pi.json", Just $ JArray [JString "π"])
  , ("y_string_simple_ascii.json", Just $ JArray [JString "asd "])
  , ("y_string_space.json", Just $ JString " ")
  , ("y_string_surrogates_U+1D11E_MUSICAL_SYMBOL_G_CLEF.json", Just $ JArray [JString "\xD834\xDd1e"])
  , ("y_string_three-byte-utf-8.json", Just $ JArray [JString "\x0821"])
  , ("y_string_two-byte-utf-8.json", Just $ JArray [JString "\x0123"])
  , ("y_string_u+2028_line_sep.json", Just $ JArray [JString "\x2028"])
  , ("y_string_u+2029_par_sep.json", Just $ JArray [JString "\x2029"])
  , ("y_string_uEscape.json", Just $ JArray [JString "aクリス"])
  , ("y_string_uescaped_newline.json", Just $ JArray [JString "new\nline"])
  , ("y_string_unescaped_char_delete.json", Just $ JArray [JString "\DEL"])
  , ("y_string_unicode.json", Just $ JArray [JString "\xA66D"])
  , ("y_string_unicodeEscapedBackslash.json", Just $ JArray [JString "\\"])
  , ("y_string_unicode_2.json", Just $ JArray [JString "⍂㈴⍂"])
  , ("y_string_unicode_escaped_double_quote.json", Just $ JArray [JString "\""])
  , ("y_string_unicode_U+10FFFE_nonchar.json", Just $ JArray [JString "\xDBFF\xDFFE"])
  , ("y_string_unicode_U+1FFFE_nonchar.json", Just $ JArray [JString "\xD83F\xDFFE"])
  , ("y_string_unicode_U+200B_ZERO_WIDTH_SPACE.json", Just $ JArray [JString "\x200B"])
  , ("y_string_unicode_U+2064_invisible_plus.json", Just $ JArray [JString "\x2064"])
  , ("y_string_unicode_U+FDD0_nonchar.json", Just $ JArray [JString "\xFDD0"])
  , ("y_string_unicode_U+FFFE_nonchar.json", Just $ JArray [JString "\xFFFE"])
  , ("y_string_utf8.json", Just $ JArray [JString "€𝄞"])
  , ("y_string_with_del_character.json", Just $ JArray [JString "a\DELa"])
  , ("y_structure_lonely_false.json", Just $ JBool False)
  , ("y_structure_lonely_int.json", Just $ JNumber 42.0)
  , ("y_structure_lonely_negative_real.json", Just $ JNumber (-0.1))
  , ("y_structure_lonely_null.json", Just $ JNull)
  , ("y_structure_lonely_string.json", Just $ JString "asd")
  , ("y_structure_lonely_true.json", Just $ JBool True)
  , ("y_structure_string_empty.json", Just $ JString "")
  , ("y_structure_trailing_newline.json",Just $ JArray [JString "a"])
  , ("y_structure_true_in_array.json", Just $ JArray [JBool True])
  , ("y_structure_whitespace_array.json",Just $ JArray [])
  ]