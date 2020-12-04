const fs = require('fs');
let input = fs.readFileSync('./input');
input = input.toString().split('\n');

function solve(input) {
	let validPasswords = [];
	for(let i = 0; i < input.length; i++) {
		input[i] = input[i].replace(/\s/gi, ""); //remove white space
		//WATCH ME OVERENGINEER THE FUCK OUT OF THIS USING THE HYPHEN AND COLON IN THE STRING TO DETERMINE WHERE ALL MY VARIABLES ARE
		let posOne = input[i].substr(0, input[i].indexOf("-"));
		let posTwo = input[i].substr(input[i].indexOf("-") + 1, input[i].indexOf(":") - 2 - input[i].indexOf("-"));
		let needle = input[i].substr(input[i].indexOf(":") - 1, 1);
		let password = input[i].substr(input[i].indexOf(":") + 1);
		let isNeedleAtPosOne = password[posOne - 1] === needle;
		let isNeedleAtPosTwo = password[posTwo - 1] === needle;
		if(isNeedleAtPosOne ^ isNeedleAtPosTwo) {
			validPasswords.push(i);
		}
	}
  return validPasswords;
	// return (validPasswords.length);
}
console.log(solve(input));
