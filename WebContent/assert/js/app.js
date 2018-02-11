var baseUrl = "/GamesWebApp/games";

function initComponents() {
	$("#start-form button").click(function () {
		var name = $("#start-form input[name=name]").val();
		var email = $("#start-form input[name=email]").val();
		var age = $("#start-form input[name=age]").val();
		// console.log(name);
		// console.log(email);
		// console.log(age);

		$.ajax({
			url: baseUrl,
			type: "GET",
			data: {
				action: "start",
				name: name,
				email: email,
				age: age
			},
			success: function(result) {
				$("#start-form input[name=message]").val(result);
			}
		});
	});
}