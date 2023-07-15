(ql:quickload '(:drakma :cl-ppcre :str))

(defparameter *pre-html* "<!DOCTYPE html>
<html lang=\"__LANG__\">
    <head>
        <!-- Google tag (gtag.js) -->
        <script async src=\"https://www.googletagmanager.com/gtag/js?id=G-SJ5CP8DCH9\"></script>
        <script>
         window.dataLayer = window.dataLayer || [];
         function gtag(){dataLayer.push(arguments);}
         gtag('js', new Date());

         gtag('config', 'G-SJ5CP8DCH9');
        </script>
        <!-- Standard Meta -->
        <meta charset=\"utf-8\" />
        <meta name=\"author\" content=\"Vito Van, Ikko Eltociear Ashimine\"/>
        <meta name=\"theme-color\" content=\"#005797\">
        <meta http-equiv=\"X-UA-Compatible\" content=\"IE=edge\" />
        <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0, maximum-scale=1.0\">
        <link href=favicon.ico rel=icon type=image/x-icon>
        <style>
body{
    background-color: #005896;
    display: flex;
    justify-content: center;
    font-family: Optima, Palatino, Charter, 'Bitstream Charter', serif;
    color: #000;
    font-size: 12pt;
    clear: both;
    padding: 0;
    margin: 0;
}
*{
    margin: 0;
    padding: 0;
}
.giscus {
    margin-top: 40px;
}
.container{
    float: left;
    width: 840px;
    padding: 20px;
}
.container > .menu > ul {
    list-style: none;
}
.container > .menu > ul > li {
    display: inline-block;
    margin: 0px 10px 5px 0px;
    padding: 0px 10px 5px 0px;
}
.container > .menu > ul > li > a{
    color: white;
}

.container img{
  border-radius: 4px;
  vertical-align: middle;
}
.panel{
    float: left;
    padding: 2em 0.9em;
    line-height: 1.2em;
    min-height: 20em;
    margin-right: 1em;
    border-right-color: blue;
    border-right-style: dashed;
    border-right-width: 0.1em;
}
.panel>ul{
    list-style:none;
}
.panel a{
    text-decoration: none;
    color: blue;
    display: block;
}
.panel>ul>li{
    margin-bottom: 0.9em;
}
.panel>ul>li.current>a{
    text-decoration: underline;
}
.panel a:hover{
    text-decoration: underline;
}
@media (max-width: 840px) {
    body {
      display: block;
    }
    .panel {
        padding: 0.9em 0.2em;
        min-height: 1em;
        width: 95%;
        border-right: none;
        border-bottom-color: blue;
        border-bottom-style: dashed;
        border-bottom-width: 0.1em;
    }
    .panel>ul>li {
        float: left;
        margin: 0 0.8em;
    }
    .container {
        width: 100%;
        padding: 0;
    }
    .markdown-body{
        min-width: inherit !important;
        border-radius: 0 !important;
    }
    .giscus {
        padding: 1%;
        width: 98%;
    }
}
.head{
    padding: 1em;
    text-transform: uppercase;
}
.head>h1>a{
    color: #888;
    text-decoration: none;
}
.content{
    padding:0 1em;
    font-size: 1em;
    line-height: 1.5em;
}
.index>a{
    display: block;
    line-height: 2em;
}
.index>li{
    margin: 0.5em 0;
    list-style-type: circle;
}
.index>li.shit{
  display: none;
}
.index>li.shit>a{
  opacity: .4;
  color: black;
}
.index>li.shit>a:visited{
  color: gray;
}
.markdown-body{
    background-color: white;
    padding: 1em;
    border-radius: 0.3em;
}

/* github-markdown.css */

.markdown-body {
  word-wrap: break-word;
}

.markdown-body a {
  background-color: transparent;
}

.markdown-body a:active,
.markdown-body a:hover {
  outline: 0;
}

.markdown-body strong {
  font-weight: bold;
}

.markdown-body img {
  border: 0;
}

.markdown-body hr {
  box-sizing: content-box;
  height: 0;
}

.markdown-body pre {
  overflow: auto;
}

.markdown-body code,
.markdown-body kbd,
.markdown-body pre {
    font-family: Consolas,'Liberation Mono',Menlo,Courier,monospace;
}

.markdown-body input {
  color: inherit;
  font: inherit;
  margin: 0;
}

.markdown-body html input[disabled] {
  cursor: default;
}

.markdown-body input {
  line-height: normal;
}

.markdown-body input[type='checkbox'] {
  box-sizing: border-box;
  padding: 0;
}

.markdown-body table {
  border-collapse: collapse;
  border-spacing: 0;
}

.markdown-body td,
.markdown-body th {
  padding: 0;
}

.markdown-body * {
  box-sizing: border-box;
}

.markdown-body input {
  font: 13px/1.4 Helvetica, arial, nimbussansl, liberationsans, freesans, clean, sans-serif, 'Segoe UI Emoji', 'Segoe UI Symbol';
}

.markdown-body a {
  color: blue;
}

.markdown-body a:link {
    text-decoration: none;
}

.markdown-body a:visited{
    color: #4078c0;
}


.markdown-body a:hover,
.markdown-body a:active {
  text-decoration: underline;
}

.markdown-body hr {
  height: 0;
  margin: 15px 0;
  overflow: hidden;
  background: transparent;
  border: 0;
  border-bottom: 1px solid #ddd;
}

.markdown-body hr:before {
  display: table;
  content: '';
}

.markdown-body hr:after {
  display: table;
  clear: both;
  content: '';
}

.markdown-body h1,
.markdown-body h2,
.markdown-body h3,
.markdown-body h4,
.markdown-body h5,
.markdown-body h6 {
  margin-top: 15px;
  margin-bottom: 15px;
  line-height: 1.1;
}

.markdown-body blockquote {
    margin: 0;
    line-height: 1em !important;
}

.markdown-body ul,
.markdown-body ol {
  padding: 0;
  margin-top: 0;
  margin-bottom: 0;
}

.markdown-body ol ol,
.markdown-body ul ol {
  list-style-type: lower-roman;
}

.markdown-body ul ul ol,
.markdown-body ul ol ol,
.markdown-body ol ul ol,
.markdown-body ol ol ol {
  list-style-type: lower-alpha;
}

.markdown-body dd {
  margin-left: 0;
}

.markdown-body pre {
  margin-top: 0;
  margin-bottom: 0;
}

.markdown-body .octicon {
  display: inline-block;
  text-decoration: none;
  text-rendering: auto;
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
  -webkit-user-select: none;
  -moz-user-select: none;
  -ms-user-select: none;
  user-select: none;
}

.markdown-body .octicon-link:before {
  content: '#';
}

.markdown-body>*:first-child {
  margin-top: 0 !important;
}

.markdown-body>*:last-child {
  margin-bottom: 0 !important;
}

.markdown-body a:not([href]) {
  color: inherit;
  text-decoration: none;
}

.markdown-body .anchor {
  position: absolute;
  top: 0;
  right: 0.5em;
  display: block;
  padding-right: 6px;
  padding-left: 30px;
  margin-left: -30px;
}

.markdown-body .anchor:focus {
  outline: none;
}

.markdown-body h1,
.markdown-body h2,
.markdown-body h3,
.markdown-body h4,
.markdown-body h5,
.markdown-body h6 {
  position: relative;
  margin-top: 1em;
  margin-bottom: 16px;
  font-weight: bold;
  line-height: 1.4;
}

.markdown-body h1 .octicon-link,
.markdown-body h2 .octicon-link,
.markdown-body h3 .octicon-link,
.markdown-body h4 .octicon-link,
.markdown-body h5 .octicon-link,
.markdown-body h6 .octicon-link {
  display: none;
  color: #000;
  vertical-align: middle;
}

.markdown-body h1:hover .anchor,
.markdown-body h2:hover .anchor,
.markdown-body h3:hover .anchor,
.markdown-body h4:hover .anchor,
.markdown-body h5:hover .anchor,
.markdown-body h6:hover .anchor {
  padding-left: 8px;
  margin-left: -30px;
  text-decoration: none;
}

.markdown-body h1:hover .anchor .octicon-link,
.markdown-body h2:hover .anchor .octicon-link,
.markdown-body h3:hover .anchor .octicon-link,
.markdown-body h4:hover .anchor .octicon-link,
.markdown-body h5:hover .anchor .octicon-link,
.markdown-body h6:hover .anchor .octicon-link {
  display: inline-block;
}

.markdown-body h1 {
  padding-bottom: 0.3em;
  font-size: 2.25em;
  line-height: 1.2;
  border-bottom: 1px solid #eee;
}

.markdown-body h1 .anchor {
  line-height: 1;
}

.markdown-body h2 {
  padding-bottom: 0.3em;
  font-size: 1.75em;
  line-height: 1.225;
  border-bottom: 1px solid #eee;
}

.markdown-body h2 .anchor {
  line-height: 1;
}

.markdown-body h3 {
  font-size: 1.5em;
  line-height: 1.43;
}

.markdown-body h3 .anchor {
  line-height: 1.2;
}

.markdown-body h4 {
  font-size: 1.25em;
}

.markdown-body h4 .anchor {
  line-height: 1.2;
}

.markdown-body h5 {
  font-size: 1em;
}

.markdown-body h5 .anchor {
  line-height: 1.1;
}

.markdown-body h6 {
  font-size: 1em;
  color: #777;
}

.markdown-body h6 .anchor {
  line-height: 1.1;
}

.markdown-body p,
.markdown-body blockquote,
.markdown-body ul,
.markdown-body ol,
.markdown-body dl,
.markdown-body table,
.markdown-body pre {
  margin-top: 0;
  margin-bottom: 16px;
}

.markdown-body hr {
  height: 4px;
  padding: 0;
  margin: 16px 0;
  background-color: #e7e7e7;
  border: 0 none;
}

.markdown-body ul,
.markdown-body ol {
  padding-left: 2em;
}

.markdown-body ul ul,
.markdown-body ul ol,
.markdown-body ol ol,
.markdown-body ol ul {
  margin-top: 0;
  margin-bottom: 0;
}

.markdown-body li>p {
  margin-top: 16px;
}

.markdown-body dl {
  padding: 0;
}

.markdown-body dl dt {
  padding: 0;
  margin-top: 16px;
  font-size: 1em;
  font-style: italic;
  font-weight: bold;
}

.markdown-body dl dd {
  padding: 0 16px;
  margin-bottom: 16px;
}

.markdown-body blockquote {
  padding: 0 15px;
  color: #777;
  border-left: 4px solid #ddd;
}

.markdown-body blockquote>:first-child {
  margin-top: 0;
}

.markdown-body blockquote>:last-child {
  margin-bottom: 0;
}

.markdown-body table {
  display: block;
  width: 100%;
  overflow: auto;
  word-break: normal;
  word-break: keep-all;
}

.markdown-body table th {
  font-weight: bold;
}

.markdown-body table th,
.markdown-body table td {
  padding: 6px 13px;
  border: 1px solid #ddd;
}

.markdown-body table tr {
  background-color: #fff;
  border-top: 1px solid #ccc;
}

.markdown-body table tr:nth-child(2n) {
  background-color: #f8f8f8;
}

.markdown-body img {
  max-width: 100%;
  box-sizing: border-box;
}

.markdown-body code {
  padding: 0;
  padding-top: 0.2em;
  padding-bottom: 0.2em;
  margin: 0;
  font-size: 85%;
  background-color: rgba(0,0,0,0.04);
  border-radius: 3px;
}

.markdown-body code:before,
.markdown-body code:after {
  letter-spacing: -0.2em;
  content: '\\00a0';
}

.markdown-body pre>code {
  padding: 0;
  margin: 0;
  font-size: 100%;
  word-break: normal;
  white-space: pre;
  background: transparent;
  border: 0;
}

.markdown-body .highlight {
  margin-bottom: 16px;
}

.markdown-body .highlight pre,
.markdown-body pre {
  padding: 16px;
  overflow: auto;
  font-size: 85%;
  line-height: 1.45;
  background-color: #f7f7f7;
  border-radius: 3px;
}

.markdown-body .highlight pre {
  margin-bottom: 0;
  word-break: normal;
}

.markdown-body pre {
  word-wrap: normal;
}

.markdown-body pre code {
  display: inline;
  max-width: initial;
  padding: 0;
  margin: 0;
  overflow: initial;
  line-height: inherit;
  word-wrap: normal;
  background-color: transparent;
  border: 0;
}

.markdown-body pre code:before,
.markdown-body pre code:after {
  content: normal;
}

.markdown-body kbd {
  display: inline-block;
  padding: 3px 5px;
  font-size: 11px;
  line-height: 10px;
  color: #555;
  vertical-align: middle;
  background-color: #fcfcfc;
  border: solid 1px #ccc;
  border-bottom-color: #bbb;
  border-radius: 3px;
  box-shadow: inset 0 -1px 0 #bbb;
}

.markdown-body .pl-c {
  color: #969896;
}

.markdown-body .pl-c1,
.markdown-body .pl-s .pl-v {
  color: #0086b3;
}

.markdown-body .pl-e,
.markdown-body .pl-en {
  color: #795da3;
}

.markdown-body .pl-s .pl-s1,
.markdown-body .pl-smi {
  color: #333;
}

.markdown-body .pl-ent {
  color: #63a35c;
}

.markdown-body .pl-k {
  color: #a71d5d;
}

.markdown-body .pl-pds,
.markdown-body .pl-s,
.markdown-body .pl-s .pl-pse .pl-s1,
.markdown-body .pl-sr,
.markdown-body .pl-sr .pl-cce,
.markdown-body .pl-sr .pl-sra,
.markdown-body .pl-sr .pl-sre {
  color: #183691;
}

.markdown-body .pl-v {
  color: #ed6a43;
}

.markdown-body .pl-id {
  color: #b52a1d;
}

.markdown-body .pl-ii {
  background-color: #b52a1d;
  color: #f8f8f8;
}

.markdown-body .pl-sr .pl-cce {
  color: #63a35c;
  font-weight: bold;
}

.markdown-body .pl-ml {
  color: #693a17;
}

.markdown-body .pl-mh,
.markdown-body .pl-mh .pl-en,
.markdown-body .pl-ms {
  color: #1d3e81;
  font-weight: bold;
}

.markdown-body .pl-mq {
  color: #008080;
}

.markdown-body .pl-mi {
  color: #333;
  font-style: italic;
}

.markdown-body .pl-mb {
  color: #333;
  font-weight: bold;
}

.markdown-body .pl-md {
  background-color: #ffecec;
  color: #bd2c00;
}

.markdown-body .pl-mi1 {
  background-color: #eaffea;
  color: #55a532;
}

.markdown-body .pl-mdr {
  color: #795da3;
  font-weight: bold;
}

.markdown-body .pl-mo {
  color: #1d3e81;
}

.markdown-body kbd {
  display: inline-block;
  padding: 3px 5px;
  font: 11px Consolas, 'Liberation Mono', Menlo, Courier, monospace;
  line-height: 10px;
  color: #555;
  vertical-align: middle;
  background-color: #fcfcfc;
  border: solid 1px #ccc;
  border-bottom-color: #bbb;
  border-radius: 3px;
  box-shadow: inset 0 -1px 0 #bbb;
}

.markdown-body .task-list-item {
  list-style-type: none;
}

.markdown-body .task-list-item+.task-list-item {
  margin-top: 3px;
}

.markdown-body .task-list-item input {
  margin: 0 0.35em 0.25em -1.6em;
  vertical-align: middle;
}

.markdown-body :checked+.radio-label {
  z-index: 1;
  position: relative;
  border-color: #4078c0;
}
</style>
        <title>CALM - Canvas Aided Lisp Magic</title>
    </head>
    <body>
        <div class=\"container\">
        <div class=\"menu\">
            <ul>
               <li><a href=\"/calm/index___LANG__.html\">$home-__LANG__</a></li>
               <li><a href=\"/calm/docs/installation___LANG__.html\">$installation-__LANG__</a></li>
               <li><a href=\"/calm/docs/hacking___LANG__.html\">$hacking-__LANG__</a></li>
               <li><a href=\"https://github.com/VitoVan/calm\">$source-__LANG__</a></li>
            </ul>
         </div>
            <div class=\"content markdown-body\">")

(defparameter *post-html* "</div>
        </div>
    </body>
</html>
")

(defun gh-markdown (md-file)
  (let ((result (drakma:http-request "https://api.github.com/markdown/raw"
                                     :method :post
                                     :content-type "text/x-markdown; charset=utf-8"
                                     :content md-file)))
    (cl-ppcre:regex-replace-all "id=\"user-content-" result "name=\"")))

(defparameter *replace-plist-for-index*
  '(
    ;; .md to .html
    "README.md" "index.html"
    "README_JA.md" "index_ja.html"
    "installation.md" "installation.html"
    "installation_JA.md" "installation_ja.html"
    "hacking.md" "hacking.html"
    "hacking_JA.md" "hacking_ja.html"
    ;; _en.html to .html
    "_en.html" ".html"
    ;; $Installation-en to Installation
    ;; $Installation-ja to インストール
    "$installation-en" "Installation"
    "$installation-ja" "インストール"
    "$hacking-en" "Hacking"
    "$hacking-ja" "ハッキング"
    "$home-en" "Home"
    "$home-ja" "ホーム"
    "$source-en" "Source"
    "$source-ja" "ソース"
    ))

(defun make-html (from to lang &key (replace-plist *replace-plist-for-index*))
  (let ((readme-html (gh-markdown (truename from))))
    (str:to-file
     to
     (str:replace-using
      replace-plist
      (str:concat
       (str:replace-all "__LANG__" lang *pre-html*)
       readme-html
       *post-html*
       )))))

(make-html "README.md" "index.html" "en")
(make-html "README_JA.md" "index_ja.html" "ja")

(make-html "docs/installation.md" "docs/installation.html" "en")
(make-html "docs/installation_JA.md" "docs/installation_ja.html" "ja")

(make-html "docs/hacking.md" "docs/hacking.html" "en")
(make-html "docs/hacking_JA.md" "docs/hacking_ja.html" "ja")

(quit)