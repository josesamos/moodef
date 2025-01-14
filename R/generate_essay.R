

#' Generate `essay` question
#'
#' @return A string.
#' @keywords internal
generate_essay <-
  function() {
    question_body <- glue::glue(
      '

    <responseformat>editor</responseformat>
    <responserequired>1</responserequired>
    <responsefieldlines>10</responsefieldlines>
    <minwordlimit></minwordlimit>
    <maxwordlimit></maxwordlimit>
    <attachments>0</attachments>
    <attachmentsrequired>0</attachmentsrequired>
    <maxbytes>0</maxbytes>
    <filetypeslist></filetypeslist>
    <graderinfo format="html">
      <text></text>
    </graderinfo>
    <responsetemplate format="html">
      <text></text>
    </responsetemplate>
'
    )

    question_body
  }
