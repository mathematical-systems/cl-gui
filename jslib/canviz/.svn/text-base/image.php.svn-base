<?php

/*
 * This file is part of Canviz. See http://www.canviz.org/
 * $Id$
 */

define('GRAPH_IMAGES_PATH', 'graphs/images');

$image = GRAPH_IMAGES_PATH . $_SERVER['PATH_INFO'];
$image = strtr($image, array('../' => ''));
$info = getimagesize($image);
$size = filesize($image);
header('Content-Type: ' . $info['mime']);
header('Length: ' . $size);
//usleep(mt_rand(500, 2000) * 1000);
readfile($image);

?>
