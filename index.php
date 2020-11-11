<?php
$topdir = "../..";
//$topdir = "../ompi-www";
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
$markdown_filename = $_GET["file"] or die("Insert some base index stuff here");
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

include_once("$topdir/includes/footer.inc");

