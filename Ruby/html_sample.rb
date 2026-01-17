
require './html.rb'
title_text = "A Blog Post"

HTML.generate (STDOUT) {
  html {
    head {
      title { title_text }}
    body {
      h1 { title_text }
      par { "This is a small paragraph" }
      ul {
        li { "One item"}
        li { "Another item" }}
      table{
        tr{
          td{"row 1, column 1"}}
        tr{
          td{"row 2, column 1"}
          td{"Last Modified : #{Time.now}"}
          td{"row 2, column 2"}}}}}}
