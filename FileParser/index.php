<?php
/**
 * Updated Stocktwits html-Parser to convert the stream-list elements.
 * Still under development!
 */
// Increase memory limit.
//ini_set('memory_limit', '900M');

$jsonData = file_get_contents("Stocktwits.html");

echo "<pre>";

preg_match_all('/data\-message-id="([0-9]+)" data\-user\-id="([0-9]+)"/', $jsonData, $ids, PREG_SET_ORDER, 0);
preg_match_all('/\'Timestamp\',.+?>(.*)<\/a>/', $jsonData, $timestamp, PREG_SET_ORDER, 0);
preg_match_all('/<div class="message-body ".+?>(.+)/', $jsonData, $message, PREG_SET_ORDER, 0);


$sumData = array();
foreach ($ids as $key => $nouse) {
    //echo $key.';'.$ids[$key][1].';'.$ids[$key][2].';'.'<br>';

    // Extracting the Bullish / Bearish - Tag from the message-body.
    preg_match_all('/<span.*?>([\s\S]*?)<\/span>/', $message[$key][0], $tag, PREG_SET_ORDER, 0);
    if(@empty($tag[0])){
        $tag = NULL;
    } else {
        $tag = $tag[0][1];
    }

    // Format timestamp (Only applicable for this dataset!).
    $timestamp[$key][1] = str_replace("Oct. 25 at", "today", $timestamp[$key][1]);

    /**
     * Replace Bullush / Bearish from text-message.
     */
    $message[$key][1] = str_replace("Bullish","", $message[$key][1]);
    $message[$key][1] = str_replace("Bearish","", $message[$key][1]);


    // Generate json-structure.
    $sumData[] = array('messageID' => $ids[$key][1], 'userID' => $ids[$key][2], 'timestamp' => date("Y-m-d H:i:s",strtotime($timestamp[$key][1])), 'message' => trim(strip_tags($message[$key][1])), 'tag' => $tag);
}

//var_dump($sumData);
var_dump(json_encode($sumData));
//file_put_contents('./stocktwits_raw.json', json_encode($sumData));

?>
