<?php
// Code to randomly generate conjoint profiles to send to a Qualtrics instance

// Terminology clarification: 
// Task = Set of choices presented to respondent in a single screen (i.e. pair of candidates)
// Profile = Single list of attributes in a given task (i.e. candidate)
// Attribute = Category characterized by a set of levels (i.e. education level)
// Level = Value that an attribute can take in a particular choice task (i.e. "no formal education")

// Attributes and Levels stored in a 2-dimensional Array 

// Function to generate weighted random numbers
function weighted_randomize($prob_array, $at_key){
  $prob_list = $prob_array[$at_key];
	
  // Create an array containing cutpoints for randomization
  $cumul_prob = array();
  $cumulative = 0.0;
  for ($i=0; $i<count($prob_list); $i++){
    $cumul_prob[$i] = $cumulative;
    $cumulative = $cumulative + floatval($prob_list[$i]);
  }

  // Generate a uniform random floating point value between 0.0 and 1.0
  $unif_rand = mt_rand() / mt_getrandmax();

  // Figure out which integer should be returned
  $outInt = 0;
  for ($k = 0; $k < count($cumul_prob); $k++){
    if ($cumul_prob[$k] <= $unif_rand){
      $outInt = $k + 1;
    }
  }

  return($outInt);
}



function weighted_randomize_race(){
	$draw = mt_rand(1, 100);
	if($draw <= 50){
		return('Black');
	} elseif($draw <= 75){
		return('Hispanic');
	} else{
		return("Asian");
	}
}

$featurearray = array(
		      "Gender" => array("Man", "Woman"),
		      "Age" => array("40", "52", "61"),
		      "Graduate degree from" => array("University of Michigan",
						      "Florida State University",
						      "Kennesaw State University",
						      "Capella University"),
		      "Prior years of relevant experience" => array("5", "10", "15"),
		      "Communication skills" => array("Moderate", "Strong", "Very Strong"),
		      "Strength of references" => array("Moderate", "Strong", "Very Strong"),
		      "Quality of writing sample" => array("Moderate", "Strong", "Very Strong"),
		      "Race/ethnicity" => array("White", "Black", "Hispanic", "Asian")
		      );

$restrictionarray = array();

$probabilityarray = array(
		      "Gender" => array(0.5, 0.5),
		      "Age" => array(0.33333, 0.33333, 0.33333),
		      "Graduate degree from" => array(0.25, 0.25, 0.25, 0.25),
		      "Prior years of relevant experience" => array(0.33333, 0.33333, 0.33333),
		      "Communication skills" => array(0.33333, 0.33333, 0.33333),
		      "Strength of references" => array(0.33333, 0.33333, 0.33333),
		      "Quality of writing sample" => array(0.33333, 0.33333, 0.33333),
		      "Race/ethnicity" => array(0.4, 0.2, 0.2, 0.2)
			  );

// Indicator for whether weighted randomization should be enabled or not
$weighted = 1;

// K = Number of tasks displayed to the respondent
$K = 20;

// N = Number of profiles displayed in each task
$N = 2;

// num_attributes = Number of Attributes in the Array
$num_attributes = count($featurearray);


$attrconstraintarray = array();


// Re-randomize the $featurearray

// Place the $featurearray keys into a new array
$featureArrayKeys = array();
$incr = 0;

foreach($featurearray as $attribute => $levels){	
  $featureArrayKeys[$incr] = $attribute;
  $incr = $incr + 1;
}

// Backup $featureArrayKeys
$featureArrayKeysBackup = $featureArrayKeys;

// If order randomization constraints exist, drop all of the non-free attributes
if (count($attrconstraintarray) != 0){
  foreach ($attrconstraintarray as $constraints){
    if (count($constraints) > 1){
      for ($p = 1; $p < count($constraints); $p++){
	if (in_array($constraints[$p], $featureArrayKeys)){
	  $remkey = array_search($constraints[$p],$featureArrayKeys);
	  unset($featureArrayKeys[$remkey]);
	}
      }
    }
  }
} 
// Re-set the array key indices
$featureArrayKeys = array_values($featureArrayKeys);
// Re-randomize the $featurearray keys
shuffle($featureArrayKeys);

// Re-insert the non-free attributes constrained by $attrconstraintarray
if (count($attrconstraintarray) != 0){
  foreach ($attrconstraintarray as $constraints){
    if (count($constraints) > 1){
      $insertloc = $constraints[0];
      if (in_array($insertloc, $featureArrayKeys)){
	$insert_block = array($insertloc);
	for ($p = 1; $p < count($constraints); $p++){
	  if (in_array($constraints[$p], $featureArrayKeysBackup)){
	    array_push($insert_block, $constraints[$p]);
	  }
	}
				
	$begin_index = array_search($insertloc, $featureArrayKeys);
	array_splice($featureArrayKeys, $begin_index, 1, $insert_block);
      }
    }
  }
}


// Re-generate the new $featurearray - label it $featureArrayNew

$featureArrayNew = array();
foreach($featureArrayKeys as $key){
  $featureArrayNew[$key] = $featurearray[$key];
}


// Initialize the array returned to the user
// Naming Convention
// Level Name: B-[task number]-[profile number]-[attribute number]
// Attribute Name: B-[task number]-[attribute number]
// Example: B-1-3-2, Returns the level corresponding to Task 1, Profile 3, Attribute 2 
// B-3-3, Returns the attribute name corresponding to Task 3, Attribute 3

$returnarray = array();

// For each task $p
for($p = 1; $p <= $K; $p++){
	weighted_randomize_race();
	$type = weighted_randomize_race();

   // Repeat until non-restricted profile generated
 	$complete = False;

  	while ($complete == False){

  // For each profile $i
  	for($i = 1; $i <= $N; $i++){


      // Create a count for $attributes to be incremented in the next loop
      $attr = 0;
			
      // Create a dictionary to hold profile's attributes
      $profile_dict = array();

      // For each attribute $attribute and level array $levels in task $p
      foreach($featureArrayNew as $attribute => $levels){	
	
	// Increment attribute count
	$attr = $attr + 1;

	if ($attribute == "Race/ethnicity"){
		$treat_number = $attr;
	}

	// Create key for attribute name
	$attr_key = "B-" . (string)$p . "-" . (string)$attr;

	// Store attribute name in $returnarray
	$returnarray[$attr_key] = $attribute;

	// Get length of $levels array
	$num_levels = count($levels);

	// Randomly select one of the level indices
	if ($weighted == 1){
	  $level_index = weighted_randomize($probabilityarray, $attribute) - 1;

	}else{
	  $level_index = mt_rand(1,$num_levels) - 1;	
	}	

	// Pull out the selected level
	$chosen_level = $levels[$level_index];
			
	// Store selected level in $profileDict
	$profile_dict[$attribute] = $chosen_level;

	// Create key for level in $returnarray
	$level_key = "B-" . (string)$p . "-" . (string)$i . "-" . (string)$attr;

	// Store selected level in $returnarray
	$returnarray[$level_key] = $chosen_level;

      }

     }

    $treat_profile_one = "B-" . (string)$p . "-1-" . (string)$treat_number;
	$treat_profile_two = "B-" . (string)$p . "-2-" . (string)$treat_number;
	$cond1 = $returnarray[$treat_profile_one]  == "White" && $returnarray[$treat_profile_two] == $type;
	$cond2 = $returnarray[$treat_profile_two]  == "White" && $returnarray[$treat_profile_one] == $type;

	if ($cond1 or $cond2){
		$complete = True;
	}

   }
 }


// Return the array back to Qualtrics
print  json_encode($returnarray);
?>
