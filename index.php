<?php
$topdir = "../..";
include_once("$topdir/doc/nav.inc");
include_once("$topdir/includes/header.inc");

// get errors to display
ini_set('display_errors', 1);
ini_set('display_startup_errors', 1);
error_reporting(E_ALL);

// include Parsedown.php and init Parsedown object
include_once("Parsedown.php");
$Parsedown = new Parsedown();

// get the filename from the get request
$markdown_filename = $_GET["file"];
// printf("Loading file: $markdown_filename"); // debug

// check that the filename is valid - TODO: make more robust
if (filesize($markdown_filename) > 1) {
        // open and read the file
        $fp = fopen($markdown_filename, "r") or die("Unable to open this markdown file");

        // read file into variable
        $markdown_file = fread($fp, filesize($markdown_filename));

        // check length of md file to make sure it is substantial
        if (strlen($markdown_file) > 2) {
                // put file into parsedown and print it to the webpage
                echo $Parsedown->text($markdown_file);
        }

        // close the file
        fclose($fp);
}
else {
        echo "$markdown_filename was found but does not have any contents";
}

include_once("$topdir/includes/footer.inc");

// old code
        // read directly into parsedown
        // echo $Parsedown->text(fread($fp, filesize($markdown_filename)));
