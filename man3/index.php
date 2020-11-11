<?php
$topdir = "../..";
$topdir = "/home/jsquyres/www/ompi-unofficial";
include_once("$topdir/doc/nav.inc");
include_once("$topdir/includes/header.inc");

// get errors to display
ini_set('display_errors', 1);
ini_set('display_startup_errors', 1);
error_reporting(E_ALL);

function error_out($error_string)
{
    print("Something terrible happened: $error_string");
    include("$topdir/includes/footer.inc");
    exit(0);
}

function emit_index_listing()
{
    $dh = opendir(".");
    if (!$dh) {
        error_out("Directory fail");
    }
    $files = array();
    while (($file = readdir($dh)) != false) {
        if (substr($file, -3) == ".md") {
            $files[] = $file;
        }
    }
    closedir($dh);

    if (count($files) > 0) {
        sort($files);
        printf("<p>The following manual pages are available:</p>\n<ul>");
        for ($i = 0; $i < count($files); ++$i) {
            printf("<li><a href=\"?file=$files[$i]\">$files[$i]</a></li>\n");
        }
        printf("</ul>\n");
    }
}

function emit_single_man_page($filename)
{
    print("I'm going to show you $filename");

    # 1. Validate the filename: Make sure it's just numbers, letters, characters.
    # 1a. If bad, error_out(...)
    # 2. Validate that the file exists
    # 2a. If file does not exist, error_out(...)
    # 3. Open and read the file and close the file
    # 3a. If bad, error_out(...)
    # 4. Make a Parsedown object and render the markdown
    # 4a. If bad, error_out(...)
    # 5. Print the rendered HTML
}

// get the filename from the get request
if (array_key_exists("file", $_GET)) {
    emit_single_man_page($_GET["file"]);
} else {
    emit_index_listing();
}


include("$topdir/includes/footer.inc");
exit(0);


// include Parsedown.php and init Parsedown object

#include_once("Parsedown.php");
#$Parsedown = new Parsedown();

// printf("Loading file: $markdown_filename"); // debug

// check that the filename is valid - TODO: make more robust
if ($markdown_filename != "") {
        // TODO: maybe change to be a subroutine
        if (file_exists($markdown_filename)) { // TODO: validate markdown_filename
                // open and read the file
                $fp = fopen($markdown_filename, "r") or die("Unable to open this markdown file");

                // read file into variable
                $markdown_file = fread($fp, filesize($markdown_filename));
                // TODO: add regex make sure there are no tabs

                echo $Parsedown->text($markdown_file);

                // close the file
                fclose($fp);
        }
        else {
                echo "File not found";
        }
}
else if ($markdown_filename == "") { // want to output index

}
else {
        echo "$markdown_filename is an invalid request...";
}
