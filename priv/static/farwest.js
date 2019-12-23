window.addEventListener('DOMContentLoaded', (event) => {
	document.querySelectorAll('form[data-operation]').forEach((form) => {
		form.addEventListener('submit', (e) => {
			const response = fetch(form.target, {
				method: form.getAttribute('method'),
				headers: {
					'content-type': form.enctype
				},
				body: form.elements[0].value,
				redirect: 'follow'
			});

			// @todo Reload or follow.

			e.preventDefault();
		});
	});
});
