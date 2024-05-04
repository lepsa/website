function setXsrfHeader(evt) {
  let token = document.cookie.split('; ').find(
    c => c.startsWith('XSRF-TOKEN')
  )?.slice('XSRF-TOKEN'.length + 1) || '';
  evt.detail.headers['X-XSRF-TOKEN'] = token;
}