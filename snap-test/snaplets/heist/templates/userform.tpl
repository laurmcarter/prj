<div class="container">
  <form class="form-signin" method="post" action="${postAction}">
    <h2 class="form-signin-heading"><form-title/></h2>
    <input type="text" class="input-block-level" name="login" placeholder="Email address">
    <input type="password" class="input-block-level" name="password" placeholder="Password">
    <label class="checkbox">
      <input type="checkbox" value="remember-me"> Remember me
    </label>
    <button class="btn btn-large btn-primary" type="submit">
      <submitText/>
    </button>
  </form>
  <form-extra/>
</div>
