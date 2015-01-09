
<!-- This is the project specific website template -->
<!-- It can be changed as liked or replaced by other content -->

<?php

$domain=ereg_replace('[^\.]*\.(.*)$','\1',$_SERVER['HTTP_HOST']);
$group_name=ereg_replace('([^\.]*)\..*$','\1',$_SERVER['HTTP_HOST']);
$themeroot='r-forge.r-project.org/themes/rforge/';

echo '<?xml version="1.0" encoding="UTF-8"?>';
?>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<!--
	Based on HTML template by TEMPLATED, http://templated.co
	Released for free under the Creative Commons Attribution License
-->
<html xmlns="http://www.w3.org/1999/xhtml">
<head>
<meta http-equiv="content-type" content="text/html; charset=utf-8" />

<title>Chapman & Feit: R for Marketing Research and Analytics</title>
<meta name="keywords" content="" />
<meta name="description" content="" />
<link href="default.css" rel="stylesheet" type="text/css" />
</head>

<body>
    
<div id="outer">
    
	<div id="header">
		<h1><a href="#">R for Marketing Research and Analytics</a></h1>
		<h2></h2>
	</div>
    
	<div id="menu">
		<ul>
			<li class="first"><a href="index.html" accesskey="1" title="">Home</a></li>
			<li><a href="data.html" accesskey="2" title="data">Data</a></li>
			<li><a href="code.html" accesskey="3" title="">Code</a></li>
            <li><a href="errata.html" accesskey="6" title="">Errata</a></li>
			<li><a href="about.html" accesskey="4" title="">About the Book</a></li>
			<li><a href="contact.html" accesskey="5" title="">Contact</a></li>
            <li><a href="faq.html" accesskey="7" title="">FAQ</a></li>
		</ul>
	</div>
    
	<div id="content">
		<div id="primaryContentContainer">
			<div id="primaryContent">
                
                <h2>Welcome to <i>R for Marketing Research and Analytics</i>!</h2>
                
                <p><strong><it>R for Marketing Research and Analytics</it></strong> is designed to teach R to marketing practitioners and data scientists. This site contains the latest, updated .R <a href="code.html" title="code">code files</a>; <a href="data.html" title="data">data files</a>; and <a href="errata.html" title="errata">errata</a>.</p>
                
                <img src="/images/chapman-feit-cover-revised.jpg" alt="Book cover" style="height:300px">

				<p></p>
                                                
				<h3>Recent Updates</h3>
				<table>
					<tr>
						<th>Date</th>
						<th>Description</th>
					</tr>
					<tr class="rowA">
						<td>January 9, 2015</td>
                        <td>Publication date: now estimated as <b>March 14, 2015</b>.</td>
					</tr>
					<tr class="rowB">
						<td>January 8, 2015</td>
						<td>This site goes live in preparation of the book release.</td>
					</tr>
				</table>
			</div>
		</div>
        
		<div id="secondaryContent">
            
			<h3>Publication Soon</h3>
			<p>Expected publication date is now March 14, 2015.</p>
            <p>Publication details are at:
 				<li><a href="http://www.springer.com/statistics/business%2C+economics+%26+finance/book/978-3-319-14435-1">Springer</a></li>
				<li><a href="http://www.amazon.com/Marketing-Research-Analytics-Use/dp/3319144359">Amazon</a></li>
                <li><a href="http://www.powells.com/biblio/62-9783319144351-1">Powell's Books</a></li>
           </p>
            
			<h3>Upcoming Events</h3>
			<p>Our workshop "Introduction to R for Marketing Researchers" will be offered at:
            <li><a href="https://www.sawtoothsoftware.com/about-us/news-and-events/events/1437-sawtooth-software-conference-2015">Sawtooth Software Conference</a>, March 23, 2015, Orlando</li>
            <li><a href="https://www.ama.org/events-training/Conferences/Pages/Advanced-Research-Techniques-%28ART%29-Forum.aspx">Advanced Research Techniques Forum</a> (ART Forum), June 14, 2015, San Diego</li>
            </p>

<!--
            <h3>Nunc pellentesque</h3>
			<p>Sed vestibulum blandit nisl. Quisque elementum convallis purus. Suspendisse potenti. Donec nulla est, laoreet quis, pellentesque in. <a href="#">More&#8230;</a></p>
            
			<h3>Ipsum Dolorem</h3>
			<ul>
				<li><a href="#">Sagittis Bibendum Erat</a></li>
				<li><a href="#">Malesuada Turpis</a></li>
				<li><a href="#">Quis Gravida Massa</a></li>
				<li><a href="#">Inerat Viverra Ornare</a></li>
			</ul>
-->
		</div>
        
		<div class="clear"></div>
	</div>
    
	<div id="footer">
		<p>Copyright &copy; 2015, Chris Chapman and Elea McDonnell Feit. Design based on <a href="http://templated.co" rel="nofollow">Rational template</a>, used under Creative Commons Attribution License.</p>
	</div>
    
</div>
    
</body>
</html>
