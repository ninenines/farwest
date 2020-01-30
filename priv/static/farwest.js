window.addEventListener('DOMContentLoaded', (event) => {
	document.querySelectorAll('form[data-operation]').forEach((form) => {
		form.addEventListener('submit', (e) => {
			fetch(form.target, {
				method: form.getAttribute('method'),
				headers: {
					'content-type': form.dataset.mediaType
				},
				body: form.elements[0].value,
				redirect: 'follow'
			})
			.then(function(response){
				if (response.ok){
					if (form.getAttribute('method') == "DELETE")
						window.location = document.querySelector("head>link[rel='parent']").href;
					else
						window.location = response.url;
				}else{
					alert("error: " + response.status + " " + response.statusText);
				}
			});

			e.preventDefault();
		});
	});
});
