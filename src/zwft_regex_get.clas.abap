class ZWFT_REGEX_GET definition
  public
  final
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !I_TEXT type STRING .
  methods GET_ACRONYMS
    returning
      value(RT_RES) type RFC_TT_STRINGS .
  methods GET_ADDRESSES
    returning
      value(RT_RES) type RFC_TT_STRINGS .
  methods GET_CREDIT_CARDS
    returning
      value(RT_RES) type RFC_TT_STRINGS .
  methods GET_DATE
    returning
      value(RT_RES) type RFC_TT_STRINGS .
  methods GET_EMAILS
    returning
      value(RT_RES) type RFC_TT_STRINGS .
  methods GET_HEX_COLORS
    returning
      value(RT_RES) type RFC_TT_STRINGS .
  methods GET_IPV4
    returning
      value(RT_RES) type RFC_TT_STRINGS .
  methods GET_LINKS
    returning
      value(RT_RES) type RFC_TT_STRINGS .
  methods GET_MONEY
    returning
      value(RT_RES) type RFC_TT_STRINGS .
  methods GET_PERCENTAGES
    returning
      value(RT_RES) type RFC_TT_STRINGS .
  methods GET_PHONES
    returning
      value(RT_RES) type RFC_TT_STRINGS .
  methods GET_TIMES
    returning
      value(RT_RES) type RFC_TT_STRINGS .
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA: m_text  TYPE string, m_regex TYPE string.
    CONSTANTS: mc_acronyms_pattern     TYPE string VALUE '\b(([A-Z]\.){2,})', "/m
               mc_addresses_pattern    TYPE string VALUE '/(\d{1,4} [\w\s]{1,20}(?:(street|avenue|road|highway|square|traill|drive|court|parkway|boulevard)\b|(st|ave|rd|hwy|sq|trl|dr|ct|pkwy|blvd)\.(?=\b)?))/im',
               mc_credit_cards_pattern TYPE string VALUE '/((?:(?:\d{4}[- ]){3}\d{4}|\d{16}))(?![\d])', "/m
               mc_date_pattern         TYPE string VALUE '((Jan(?:uary)?|Feb(?:ruary)?|Mar(?:ch)?|Apr(?:il)?|May|Jun(?:e)?|Jul(?:y)?|Aug(?:ust)?|Sep(?:tember)?|Oct(?:ober)?|(?:Nov|Dec)(?:ember)?)\D?(\d{1,2}\D?)?\D?((?:19[7-9]\d|20\d{2})|\d{2}))',
               mc_emails_pattern       TYPE string VALUE '(\w+(\.\w+)*@(\w+\.)+(\w{2,4}))',
               mc_hex_colors_pattern   TYPE string VALUE '(#(?:[0-9a-fA-F]{3}){1,2})',
               mc_ipv4_pattern         TYPE string VALUE '\b(((25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.){3}(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?))',
              " mc_ipv6_pattern      TYPE string VALUE ' /((([0-9A-Fa-f]{1,4}:){7}[0-9A-Fa-f]{1,4})|(([0-9A-Fa-f]{1,4}:){6}:[0-9A-Fa-f]{1,4})|(([0-9A-Fa-f]{1,4}:){5}:([0-9A-Fa-f]{1,4}:)?[0-9A-Fa-f]{1,4})|(([0-9A-Fa-f]{1,4}:){4}:([0-9A-Fa-f]{1,4}:){0,2}[0-
               mc_links_pattern        TYPE string VALUE '((https?|ftp|gopher|telnet|file):((//)|(\\\\\\\\))+[\\\\w\\\\d:#@%/;$()~_?\\\\+-=\\\\\\\\\\\\.&]*)',
               mc_money_pattern        TYPE string VALUE '(((^|\b)US?)?\$\s?[0-9]{1,3}((,[0-9]{3})+|([0-9]{3})+)?(\.[0-9]{1,2})?\b)', "/m
               mc_percentages_pattern  TYPE string VALUE '((100(\.0+)?|[0-9]{1,2}(\.[0-9]+)?)%)',
               mc_phones_pattern       TYPE string VALUE '(\d?[^\s\w]*(?:\(?\d{3}\)?\W*)?\d{3}\W*\d{4})', "/im
               mc_times_pattern        TYPE string VALUE '\b((0?[0-9]|1[0-2])(:[0-5][0-9])?(AM|PM)|([01]?[0-9]|2[0-3]):[0-5][0-9])'.
    METHODS get_regex IMPORTING i_pattern TYPE string RETURNING VALUE(rt_res) TYPE rfc_tt_strings.


ENDCLASS.



CLASS ZWFT_REGEX_GET IMPLEMENTATION.


  METHOD CONSTRUCTOR.
    m_text = i_text.
  ENDMETHOD.


  METHOD GET_ACRONYMS.
    rt_res = get_regex( mc_acronyms_pattern ).
  ENDMETHOD.


  METHOD GET_ADDRESSES.
    rt_res = get_regex( mc_addresses_pattern ).
  ENDMETHOD.


  METHOD GET_CREDIT_CARDS.
    rt_res = get_regex( mc_credit_cards_pattern ).
  ENDMETHOD.


  METHOD GET_DATE.
    rt_res = get_regex( mc_date_pattern ).

  ENDMETHOD.


  METHOD GET_EMAILS.
    rt_res = get_regex( mc_emails_pattern ).
  ENDMETHOD.


  METHOD GET_HEX_COLORS.
    rt_res = get_regex( mc_hex_colors_pattern ).
  ENDMETHOD.


  METHOD GET_IPV4.
    rt_res = get_regex( mc_ipv4_pattern ).
  ENDMETHOD.


  METHOD GET_LINKS.
    " rt_res = get_regex( mc_links_pattern ).

    DATA(lo_regex) = NEW cl_abap_regex( pattern = mc_links_pattern ).
    DATA(lo_matcher) = NEW cl_abap_matcher( regex = lo_regex text = m_text ).
    WHILE lo_matcher->find_next( ) = abap_true.
      TRY.
          APPEND lo_matcher->get_submatch( index = sy-tabix ) TO rt_res.
          lo_matcher->get_length( index = 1 ).

        CATCH cx_sy_matcher.
      ENDTRY.
    ENDWHILE.

  ENDMETHOD.


  METHOD GET_MONEY.
    rt_res = get_regex( mc_money_pattern ).
  ENDMETHOD.


  METHOD GET_PERCENTAGES.
    rt_res = get_regex( mc_percentages_pattern ).
  ENDMETHOD.


  METHOD GET_PHONES.
    rt_res = get_regex( mc_phones_pattern ).
  ENDMETHOD.


  METHOD GET_REGEX.

    DATA(lo_regex) = NEW cl_abap_regex( pattern = i_pattern ).
    DATA(lo_matcher) = NEW cl_abap_matcher( regex = lo_regex text = m_text ).
    WHILE lo_matcher->find_next( ) = abap_true.
      TRY.
          APPEND lo_matcher->get_submatch( index = 1 ) TO rt_res.
          lo_matcher->get_length( index = 1 ).

        CATCH cx_sy_matcher.
      ENDTRY.
    ENDWHILE.

  ENDMETHOD.


  METHOD GET_TIMES.
    rt_res = get_regex( mc_times_pattern ).
  ENDMETHOD.
ENDCLASS.
